import { useState, useEffect } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import { createInvoice, setUserKeyHash, getUserKeyHash, getHeads, getRegisteredHead, getRelayGraph, type InvoiceResponse, type RegisteredHeadDetail } from '../api/client'
import { useWallet } from '../context/WalletContext'
import { useUser } from '../context/UserContext'
import { useNetwork } from '../context/NetworkContext'

export default function CreateInvoice() {
  const { address: walletAddress } = useWallet()
  const { setPendingInvoice } = useUser()
  const { network } = useNetwork()
  const [heads, setHeads] = useState<RegisteredHeadDetail[]>([])
  const [selectedHeadId, setSelectedHeadId] = useState('')
  const [routingHeadCount, setRoutingHeadCount] = useState<number | null>(null)
  const [receiverKeyHash, setReceiverKeyHash] = useState('')
  const [paymentHash, setPaymentHash] = useState('')
  const [amountAda, setAmountAda] = useState('')
  const [memo, setMemo] = useState('')
  const [expiresMinutes, setExpiresMinutes] = useState('60')
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState<InvoiceResponse | null>(null)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    getHeads()
      .then(hs => Promise.all(hs.map(h => getRegisteredHead(h.headId).catch(() => null))))
      .then(details => {
        const htlc = details.filter((d): d is RegisteredHeadDetail => d !== null && d.htlcEnabled)
        setHeads(htlc)
        if (htlc.length === 1) setSelectedHeadId(htlc[0].headId)
      })
      .catch(() => {})
  }, [])

  useEffect(() => {
    if (!selectedHeadId || network === 'All') { setRoutingHeadCount(null); return }
    getRelayGraph(network)
      .then(g => {
        const neighbours = new Set<string>()
        g.edges.forEach(e => {
          if (e.fromHead === selectedHeadId) neighbours.add(e.toHead)
          if (e.toHead === selectedHeadId) neighbours.add(e.fromHead)
        })
        setRoutingHeadCount(neighbours.size)
      })
      .catch(() => setRoutingHeadCount(null))
  }, [selectedHeadId, network])

  useEffect(() => {
    if (!walletAddress) return
    getUserKeyHash(walletAddress)
      .then(({ keyHash }) => { if (keyHash) setReceiverKeyHash(keyHash) })
      .catch(() => {})
  }, [walletAddress])

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (loading || result) return
    setLoading(true)
    setError(null)
    setResult(null)

    if (!selectedHeadId) {
      setError('Select a head')
      setLoading(false)
      return
    }

    const amountLovelace = Math.round(parseFloat(amountAda) * 1_000_000)
    if (isNaN(amountLovelace) || amountLovelace <= 0) {
      setError('Amount must be a positive number')
      setLoading(false)
      return
    }

    if (!paymentHash.match(/^[0-9a-fA-F]{64}$/)) {
      setError('Payment hash must be a 64-character hex string')
      setLoading(false)
      return
    }

    if (!receiverKeyHash.match(/^[0-9a-fA-F]{56}$/)) {
      setError('Receiver key hash must be a 56-character hex string (28-byte vkey hash)')
      setLoading(false)
      return
    }

    try {
      const res = await createInvoice({
        headId: selectedHeadId,
        receiverOnChainId: receiverKeyHash,
        paymentHash,
        amountLovelace,
        memo: memo || undefined,
        expiresInSeconds: parseInt(expiresMinutes) * 60,
      })
      setPendingInvoice(res)
      if (walletAddress) setUserKeyHash(walletAddress, receiverKeyHash).catch(() => {})
      setResult(res)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to create invoice')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="invoice-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h2 className="section-title">How HTLC Payments Work</h2>
        <div className="next-steps">
          <div className="next-step">
            <span className="next-num">1</span>
            <p>Generate a secret offline and compute its BLAKE2b-256 hash. Create an invoice with the hash &mdash; the secret never leaves your machine.</p>
          </div>
          <div className="next-step">
            <span className="next-num">2</span>
            <p>Share the invoice ID with the sender. They find a route through bridge operators and lock funds in HTLC contracts along the path.</p>
          </div>
          <div className="next-step">
            <span className="next-num">3</span>
            <p>Reveal the secret to claim your funds. Bridge operators use the revealed secret to claim their locked funds in sequence.</p>
          </div>
        </div>

        <h1 className="section-title" style={{ marginTop: '4rem' }}>Create Payment Invoice</h1>
        <p className="register-desc">
          Generate an invoice to receive a cross-head payment via HTLC relay.
          You provide a payment hash &mdash; the BLAKE2b-256 hash of a secret that only you know.
          The secret never touches this service.
        </p>

        <form className="register-form" onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="headId">Head</label>
            {heads.length === 0 ? (
              <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)' }}>
                No HTLC-enabled heads found. <Link to="/register" style={{ color: 'var(--accent)' }}>Register and publish the HTLC validator first.</Link>
              </p>
            ) : (
              <>
                <select
                  id="headId"
                  value={selectedHeadId}
                  onChange={e => setSelectedHeadId(e.target.value)}
                  required
                  disabled={loading || result !== null}
                  style={{ width: '100%' }}
                >
                  <option value="">Select a head…</option>
                  {heads.map(h => (
                    <option key={h.headId} value={h.headId}>{h.headId}</option>
                  ))}
                </select>
                {selectedHeadId && routingHeadCount !== null && (
                  <span className="form-hint" style={{ color: routingHeadCount === 0 ? 'var(--error)' : 'var(--success)', marginTop: '0.35rem', display: 'block' }}>
                    {routingHeadCount === 0
                      ? 'No other heads can route to this head yet — share a bridge participant with another head first.'
                      : `${routingHeadCount} head${routingHeadCount === 1 ? '' : 's'} can route payments to this head.`}
                  </span>
                )}
              </>
            )}
          </div>
          <div className="form-group">
            <label htmlFor="receiverKeyHash">Your Cardano Key Hash (28 bytes hex)</label>
            <input
              id="receiverKeyHash"
              type="text"
              placeholder="56-char hex"
              value={receiverKeyHash}
              onChange={e => setReceiverKeyHash(e.target.value)}
              required
              disabled={loading || result !== null}
            />
            <span className="form-hint">
              Hash of your hydra-node's <code>--cardano-signing-key</code> verification key — your participant identity in the head.
              See <Link to="/setup" style={{ color: 'var(--accent)' }}>Setup guide → step 02</Link>.
            </span>
          </div>
          <div className="form-group">
            <label htmlFor="paymentHash">Payment Hash</label>
            <input
              id="paymentHash"
              type="text"
              placeholder="64-character hex (BLAKE2b-256 of your secret)"
              value={paymentHash}
              onChange={e => setPaymentHash(e.target.value)}
              required
              disabled={loading || result !== null}
            />
            <span className="form-hint">
              Generate a secret offline (e.g. <code>openssl rand -hex 32</code>),
              then hash it
              (<code>echo -n SECRET | xxd -r -p | b2sum -l 256</code>).
              Paste only the hash here.
            </span>
          </div>
          <div className="form-row">
            <div className="form-group">
              <label htmlFor="amountAda">Amount (ADA)</label>
              <input
                id="amountAda"
                type="text"
                placeholder="e.g. 100"
                value={amountAda}
                onChange={e => setAmountAda(e.target.value)}
                required
                disabled={loading || result !== null}
              />
            </div>
            <div className="form-group">
              <label htmlFor="expiresMinutes">Expires in (minutes)</label>
              <input
                id="expiresMinutes"
                type="number"
                min="1"
                value={expiresMinutes}
                onChange={e => setExpiresMinutes(e.target.value)}
                disabled={loading || result !== null}
              />
            </div>
          </div>
          <div className="form-group">
            <label htmlFor="memo">Memo (optional)</label>
            <input
              id="memo"
              type="text"
              placeholder="Payment for..."
              value={memo}
              onChange={e => setMemo(e.target.value)}
              disabled={loading || result !== null}
            />
          </div>
          {result === null && (
            <button type="submit" className="btn btn-primary btn-full" disabled={loading || heads.length === 0}>
              {loading ? 'Creating…' : 'Create Invoice'}
            </button>
          )}
        </form>

        {result && (
          <motion.div
            className="register-result success"
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 0.3 }}
          >
            <h3>Invoice Created</h3>
            <div className="result-details">
              <div className="result-row">
                <span className="result-label">Invoice ID</span>
                <code className="result-value">{result.invoiceId}</code>
              </div>
              <div className="result-row">
                <span className="result-label">Amount</span>
                <span className="result-value">{(result.amountLovelace / 1_000_000).toFixed(6)} ADA</span>
              </div>
              <div className="result-row">
                <span className="result-label">Status</span>
                <span className="result-value">{result.status}</span>
              </div>
              <div className="result-row">
                <span className="result-label">Expires</span>
                <span className="result-value">{new Date(result.expiresAt).toLocaleString()}</span>
              </div>
            </div>
            <p className="invoice-share-hint">
              Share the <strong>invoice ID</strong> with the sender so they can find a route and pay.
            </p>
          </motion.div>
        )}

        {error && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 0.3 }}
          >
            <h3>Error</h3>
            <p>{error}</p>
          </motion.div>
        )}
      </motion.section>

    </div>
  )
}
