import { useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
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
      setError('Payment hash must be a 64-character hex string (SHA-256 of your secret)')
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
          You must provide the SHA-256 hash of a secret you hold &mdash; the sender
          will use this hash to lock funds along the route.
        </p>

        <form className="register-form" onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="receiverAddress">Receiver Address</label>
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
            <label htmlFor="paymentHash">Payment Hash (SHA-256 hex)</label>
            <input
              id="paymentHash"
              type="text"
              placeholder="e.g. a1b2c3d4..."
              value={paymentHash}
              onChange={e => setPaymentHash(e.target.value)}
              required
            />
            <span className="form-hint">Hash of your secret. Keep the secret safe &mdash; reveal it to claim funds.</span>
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
              Share this invoice ID with the sender so they can find a route and pay.
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
            <p>You generate a secret and create an invoice with its SHA-256 hash. The secret stays private.</p>
          </div>
          <div className="next-step">
            <span className="next-num">2</span>
            <p>The sender finds a route through bridge operators and locks funds in HTLC contracts along the path.</p>
          </div>
          <div className="next-step">
            <span className="next-num">3</span>
            <p>You reveal the secret to claim your funds. Bridge operators use the revealed secret to claim their locked funds in sequence.</p>
          </div>
        </div>
      </motion.section>
    </div>
  )
}
