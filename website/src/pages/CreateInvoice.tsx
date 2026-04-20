import { useState } from 'react'
import { motion } from 'framer-motion'
import { createInvoice, type InvoiceResponse } from '../api/client'

export default function CreateInvoice() {
  const [receiverAddress, setReceiverAddress] = useState('')
  const [paymentHash, setPaymentHash] = useState('')
  const [amountAda, setAmountAda] = useState('')
  const [memo, setMemo] = useState('')
  const [expiresMinutes, setExpiresMinutes] = useState('60')
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState<InvoiceResponse | null>(null)
  const [error, setError] = useState<string | null>(null)

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)
    setError(null)
    setResult(null)

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

    try {
      const res = await createInvoice({
        receiverAddress,
        paymentHash,
        amountLovelace,
        memo: memo || undefined,
        expiresInSeconds: parseInt(expiresMinutes) * 60,
      })
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
        <h1 className="section-title">Create Payment Invoice</h1>
        <p className="register-desc">
          Generate an invoice to receive a cross-head payment via HTLC relay.
          You provide a payment hash &mdash; the SHA-256 of a secret that only you know.
          The secret never touches this service.
        </p>

        <form className="register-form" onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="receiverAddress">Your Address (Receiver)</label>
            <input
              id="receiverAddress"
              type="text"
              placeholder="addr1q..."
              value={receiverAddress}
              onChange={e => setReceiverAddress(e.target.value)}
              required
            />
          </div>
          <div className="form-group">
            <label htmlFor="paymentHash">Payment Hash</label>
            <input
              id="paymentHash"
              type="text"
              placeholder="64-character hex (SHA-256 of your secret)"
              value={paymentHash}
              onChange={e => setPaymentHash(e.target.value)}
              required
            />
            <span className="form-hint">
              Generate a secret offline (e.g. <code>openssl rand -hex 32</code>),
              then hash it (<code>echo -n SECRET | sha256sum</code>).
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
            />
          </div>
          <button type="submit" className="btn btn-primary btn-full" disabled={loading}>
            {loading ? 'Creating...' : 'Create Invoice'}
          </button>
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

      <motion.section
        className="section"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.3, duration: 0.5 }}
      >
        <h2 className="section-title">How HTLC Payments Work</h2>
        <div className="next-steps">
          <div className="next-step">
            <span className="next-num">1</span>
            <p>Generate a secret offline and compute its SHA-256 hash. Create an invoice with the hash &mdash; the secret never leaves your machine.</p>
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
      </motion.section>
    </div>
  )
}
