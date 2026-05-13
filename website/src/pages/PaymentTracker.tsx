import { useEffect, useState } from 'react'
import { useParams, Link } from 'react-router-dom'
import { motion } from 'framer-motion'
import { getPaymentStatus, submitPreimage, type PaymentStatusResponse } from '../api/client'

const hopStatusColors: Record<string, string> = {
  pending: 'var(--text-muted)',
  locked: '#f0c040',
  claimed: 'var(--success)',
  refunded: 'var(--error)',
  expired: 'var(--error)',
}

export default function PaymentTracker() {
  const { paymentId } = useParams<{ paymentId: string }>()
  const [payment, setPayment] = useState<PaymentStatusResponse | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [preimageInput, setPreimageInput] = useState('')
  const [revealing, setRevealing] = useState(false)
  const [revealOk, setRevealOk] = useState(false)
  const [revealErr, setRevealErr] = useState<string | null>(null)

  const fetchStatus = () => {
    if (!paymentId) return
    getPaymentStatus(paymentId)
      .then(setPayment)
      .catch(e => setError(e.message))
      .finally(() => setLoading(false))
  }

  useEffect(() => {
    fetchStatus()
    const interval = setInterval(fetchStatus, 5000)
    return () => clearInterval(interval)
  }, [paymentId])

  if (loading) {
    return (
      <div className="payment-page">
        <section className="section">
          <div className="stats-loading">
            <div className="loading-spinner" />
            <p>Loading payment status...</p>
          </div>
        </section>
      </div>
    )
  }

  if (error || !payment) {
    return (
      <div className="payment-page">
        <section className="section">
          <h1 className="section-title">Payment Not Found</h1>
          <div className="register-result error">
            <p>{error ?? 'Payment not found'}</p>
          </div>
        </section>
      </div>
    )
  }

  const overallColor = payment.status === 'completed'
    ? 'var(--success)'
    : payment.status === 'failed' || payment.status === 'expired'
      ? 'var(--error)'
      : '#f0c040'

  return (
    <div className="payment-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Payment Tracker</h1>

        {/* What's next callout */}
        {payment.status === 'in_progress' && (
          <div className="glow-card" style={{ marginBottom: '1.5rem', padding: '1.25rem', borderColor: '#f0c040' }}>
            <h3 style={{ color: '#f0c040', margin: '0 0 0.5rem' }}>What's next?</h3>
            <p className="register-desc" style={{ margin: '0 0 1rem' }}>
              Each hop in this payment requires a signed transaction submitted to the head. Open your
              Dashboard to see exactly which actions are available to you right now.
            </p>
            <Link to="/dashboard" className="btn btn-primary" style={{ display: 'inline-block' }}>
              Open Dashboard →
            </Link>
          </div>
        )}

        {/* Preimage reveal for receivers */}
        {payment.status === 'in_progress' && payment.hops.some(h => h.htlcStatus === 'locked') && (
          <div className="glow-card" style={{ marginBottom: '1.5rem', padding: '1.25rem' }}>
            <h3 style={{ margin: '0 0 0.5rem' }}>Reveal your secret (receivers)</h3>
            <p className="register-desc" style={{ margin: '0 0 0.75rem' }}>
              If you are the receiver of this payment, submit your secret here. This reveals the
              preimage to all bridge operators and unlocks the cascade.
            </p>
            {!revealOk ? (
              <>
                <div className="form-group" style={{ marginBottom: '0.5rem' }}>
                  <input
                    type="text"
                    placeholder="Your secret hex (from openssl rand -hex 32)"
                    value={preimageInput}
                    onChange={e => setPreimageInput(e.target.value)}
                  />
                </div>
                {revealErr && <p style={{ color: 'var(--error)', fontSize: '0.85rem', marginBottom: '0.5rem' }}>{revealErr}</p>}
                <button
                  className="btn btn-primary"
                  onClick={async () => {
                    const lastHop = payment.hops[payment.hops.length - 1]
                    setRevealErr(null)
                    setRevealing(true)
                    try {
                      await submitPreimage(lastHop.secretHash, preimageInput.trim())
                      setRevealOk(true)
                    } catch (e) {
                      setRevealErr(e instanceof Error ? e.message : 'Failed to reveal')
                    } finally {
                      setRevealing(false)
                    }
                  }}
                  disabled={revealing || !preimageInput.trim()}
                >
                  {revealing ? 'Revealing…' : 'Reveal Secret'}
                </button>
              </>
            ) : (
              <p style={{ color: 'var(--success)', margin: 0 }}>
                Secret revealed — bridge operators have been notified.
              </p>
            )}
          </div>
        )}

        <div className="payment-overview glow-card">
          <div className="payment-status-badge" style={{ borderColor: overallColor, color: overallColor }}>
            {payment.status.toUpperCase()}
          </div>

          <div className="payment-details">
            <div className="result-row">
              <span className="result-label">Payment ID</span>
              <code className="result-value">{payment.routeId}</code>
            </div>
            <div className="result-row">
              <span className="result-label">Invoice</span>
              <code className="result-value">{payment.invoiceId}</code>
            </div>
            <div className="result-row">
              <span className="result-label">Amount</span>
              <span className="result-value">{(payment.amountLovelace / 1_000_000).toFixed(6)} ADA</span>
            </div>
            <div className="result-row">
              <span className="result-label">Total Fee</span>
              <span className="result-value">{(payment.totalFee / 1_000_000).toFixed(6)} ADA</span>
            </div>
            <div className="result-row">
              <span className="result-label">Network</span>
              <span className="result-value">{payment.network}</span>
            </div>
            <div className="result-row">
              <span className="result-label">Sender</span>
              <code className="result-value">{payment.senderAddress.slice(0, 20)}...{payment.senderAddress.slice(-8)}</code>
            </div>
            <div className="result-row">
              <span className="result-label">Receiver</span>
              <code className="result-value">{payment.receiverAddress.slice(0, 20)}...{payment.receiverAddress.slice(-8)}</code>
            </div>
            <div className="result-row">
              <span className="result-label">Created</span>
              <span className="result-value">{new Date(payment.createdAt).toLocaleString()}</span>
            </div>
            <div className="result-row">
              <span className="result-label">Updated</span>
              <span className="result-value">{new Date(payment.updatedAt).toLocaleString()}</span>
            </div>
          </div>
        </div>

        <h2 className="section-title" style={{ marginTop: '3rem' }}>HTLC Hop Status</h2>
        <p className="explorer-desc">
          Each hop represents an HTLC contract between bridge operators. Funds flow left to right.
          Auto-refreshing every 5 seconds.
        </p>

        <div className="hops-timeline">
          {payment.hops.map((hop, i) => (
            <motion.div
              key={i}
              className="hop-timeline-item"
              initial={{ opacity: 0, x: -20 }}
              animate={{ opacity: 1, x: 0 }}
              transition={{ delay: i * 0.15, duration: 0.4 }}
            >
              <div
                className="hop-timeline-dot"
                style={{ background: hopStatusColors[hop.htlcStatus] ?? 'var(--text-muted)' }}
              />
              {i < payment.hops.length - 1 && (
                <div
                  className="hop-timeline-line"
                  style={{
                    background: hop.htlcStatus === 'claimed' ? 'var(--success)' : 'var(--border)',
                  }}
                />
              )}
              <div className="hop-timeline-card glow-card">
                <div className="hop-timeline-header">
                  <span className="hop-timeline-index">Hop {hop.hopIndex + 1}</span>
                  <span
                    className="hop-timeline-status"
                    style={{ color: hopStatusColors[hop.htlcStatus] ?? 'var(--text-muted)' }}
                  >
                    {hop.htlcStatus}
                  </span>
                </div>
                <div className="hop-timeline-body">
                  <div className="meta-row">
                    <span className="meta-label">Head</span>
                    <code className="meta-value">{hop.headId.slice(0, 12)}...</code>
                  </div>
                  <div className="meta-row">
                    <span className="meta-label">Sender</span>
                    <code className="meta-value">{hop.senderAddress.slice(0, 16)}...</code>
                  </div>
                  <div className="meta-row">
                    <span className="meta-label">Receiver</span>
                    <code className="meta-value">{hop.receiverAddress.slice(0, 16)}...</code>
                  </div>
                  <div className="meta-row">
                    <span className="meta-label">Bridge</span>
                    <code className="meta-value">{hop.bridgeAddress.slice(0, 16)}...</code>
                  </div>
                  <div className="meta-row">
                    <span className="meta-label">Fee</span>
                    <span className="meta-value">{(hop.fee / 1_000_000).toFixed(6)} ADA</span>
                  </div>
                  <div className="meta-row">
                    <span className="meta-label">Timeout Slot</span>
                    <span className="meta-value">{hop.timeoutSlot}</span>
                  </div>
                  {hop.preimage && (
                    <div className="meta-row">
                      <span className="meta-label">Preimage</span>
                      <code className="meta-value">{hop.preimage.slice(0, 16)}...</code>
                    </div>
                  )}
                  {hop.htlcTxHash && (
                    <div className="meta-row">
                      <span className="meta-label">HTLC Tx</span>
                      <code className="meta-value">{hop.htlcTxHash.slice(0, 16)}...</code>
                    </div>
                  )}
                  {hop.lockedAt && (
                    <div className="meta-row">
                      <span className="meta-label">Locked</span>
                      <span className="meta-value">{new Date(hop.lockedAt).toLocaleTimeString()}</span>
                    </div>
                  )}
                  {hop.claimedAt && (
                    <div className="meta-row">
                      <span className="meta-label">Claimed</span>
                      <span className="meta-value">{new Date(hop.claimedAt).toLocaleTimeString()}</span>
                    </div>
                  )}
                </div>
              </div>
            </motion.div>
          ))}
        </div>
      </motion.section>
    </div>
  )
}
