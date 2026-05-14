import { useState, useCallback, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { Link } from 'react-router-dom'
import {
  getParticipantRoutes,
  buildLockTx,
  buildClaimTx,
  buildRefundTx,
  submitTx,
  submitPreimage,
  getUserKeyHash,
  setUserKeyHash,
  type ParticipantRouteSummary,
  type ParticipantAction,
  type HopStatusResponse,
} from '../api/client'
import { useWallet } from '../context/WalletContext'

// ─── helpers ──────────────────────────────────────────────────────────────

const urgencyColor: Record<string, string> = {
  ok: 'var(--text-muted)',
  soon: '#f0c040',
  expiring: '#ff8c00',
  expired: 'var(--error)',
}

const hopStatusColor: Record<string, string> = {
  pending: 'var(--text-muted)',
  locked: '#f0c040',
  claimed: 'var(--success)',
  refunded: 'var(--error)',
}

function roleBadge(role: string) {
  const colors: Record<string, string> = {
    sender: '#4a9eff',
    bridge: '#f0c040',
    receiver: 'var(--success)',
  }
  return (
    <span
      key={role}
      style={{
        display: 'inline-block',
        padding: '0.15rem 0.5rem',
        borderRadius: '3px',
        fontSize: '0.75rem',
        fontFamily: 'var(--font-mono)',
        border: `1px solid ${colors[role] ?? 'var(--border)'}`,
        color: colors[role] ?? 'var(--text-dim)',
        marginRight: '0.4rem',
      }}
    >
      {role}
    </span>
  )
}

// ─── TxPanel ──────────────────────────────────────────────────────────────

interface TxPanelProps {
  action: ParticipantAction
  routeId: string
  headId: string
  secretHash: string
  onDone: () => void
}

function TxPanel({ action, routeId, headId, secretHash, onDone }: TxPanelProps) {
  const [walletAddress, setWalletAddress] = useState('')
  const [preimage, setPreimage] = useState('')
  const [building, setBuilding] = useState(false)
  const [builtCbor, setBuiltCbor] = useState<string | null>(null)
  const [signedCbor, setSignedCbor] = useState('')
  const [submitting, setSubmitting] = useState(false)
  const [submitOk, setSubmitOk] = useState(false)
  const [err, setErr] = useState<string | null>(null)
  const [copied, setCopied] = useState(false)

  // For receivers: also submit the preimage to the registry before/after claiming
  const [revealingPreimage, setRevealingPreimage] = useState(false)

  const handleBuild = async () => {
    setErr(null)
    setBuilding(true)
    try {
      let result
      if (action.kind === 'lock') {
        result = await buildLockTx(routeId, action.hopIndex, walletAddress)
      } else if (action.kind === 'claim') {
        result = await buildClaimTx(routeId, action.hopIndex, walletAddress, preimage)
        // Also reveal preimage to the registry so upstream bridges learn it
        setRevealingPreimage(true)
        try {
          await submitPreimage(secretHash, preimage)
        } catch {
          // Non-fatal: the preimage reveal may have already been submitted
        } finally {
          setRevealingPreimage(false)
        }
      } else {
        result = await buildRefundTx(routeId, action.hopIndex, walletAddress)
      }
      setBuiltCbor(result.cborHex)
    } catch (e) {
      setErr(e instanceof Error ? e.message : 'Failed to build transaction')
    } finally {
      setBuilding(false)
    }
  }

  const handleCopy = () => {
    if (!builtCbor) return
    navigator.clipboard.writeText(builtCbor).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    })
  }

  const handleSubmit = async () => {
    setErr(null)
    setSubmitting(true)
    try {
      const result = await submitTx(headId, signedCbor.trim())
      if (result.status === 'TxInvalid') {
        setErr(`Transaction rejected by head: ${result.error ?? 'unknown reason'}`)
      } else {
        setSubmitOk(true)
        setTimeout(onDone, 1500)
      }
    } catch (e) {
      setErr(e instanceof Error ? e.message : 'Submit failed')
    } finally {
      setSubmitting(false)
    }
  }

  const actionLabel = action.kind === 'lock' ? 'Lock HTLC' : action.kind === 'claim' ? 'Claim HTLC' : 'Refund HTLC'

  if (submitOk) {
    return (
      <div className="register-result success" style={{ marginTop: '1rem' }}>
        <p>Transaction submitted successfully. Refreshing…</p>
      </div>
    )
  }

  return (
    <div style={{ marginTop: '1rem', paddingTop: '1rem', borderTop: '1px solid var(--border)' }}>
      {!builtCbor ? (
        <>
          <p className="register-desc" style={{ marginBottom: '1rem' }}>
            {action.kind === 'lock' && 'Lock funds in the HTLC contract inside this head.'}
            {action.kind === 'claim' && 'Claim the locked funds by revealing your secret preimage.'}
            {action.kind === 'refund' && 'Recover your locked funds — the timeout has passed.'}
          </p>

          <div className="form-group" style={{ marginBottom: '0.75rem' }}>
            <label>Your L2 wallet address (bech32)</label>
            <input
              type="text"
              placeholder="addr_test1..."
              value={walletAddress}
              onChange={e => setWalletAddress(e.target.value)}
            />
            <span className="form-hint">Your address inside this Hydra head — where change and output go.</span>
          </div>

          {action.kind === 'claim' && (
            <div className="form-group" style={{ marginBottom: '0.75rem' }}>
              <label>Secret preimage (hex)</label>
              <input
                type="text"
                placeholder="32-byte hex — the secret you generated when creating the invoice"
                value={preimage}
                onChange={e => setPreimage(e.target.value)}
              />
              <span className="form-hint">
                This is the secret from <code>openssl rand -hex 32</code> you used when creating the invoice.
                Submitting it also notifies all bridge operators automatically.
              </span>
            </div>
          )}

          {err && <div className="register-result error" style={{ marginBottom: '0.75rem' }}><p>{err}</p></div>}

          <button
            className="btn btn-primary"
            onClick={handleBuild}
            disabled={building || revealingPreimage || !walletAddress || (action.kind === 'claim' && !preimage)}
          >
            {building ? 'Building…' : revealingPreimage ? 'Notifying bridges…' : `Build ${actionLabel} Transaction`}
          </button>
        </>
      ) : (
        <>
          <p className="register-desc" style={{ marginBottom: '0.5rem' }}>
            <strong>Step 1 — Sign this transaction offline</strong>
          </p>
          <div style={{ position: 'relative' }}>
            <pre className="code-block" style={{ wordBreak: 'break-all', whiteSpace: 'pre-wrap', maxHeight: '8rem', overflow: 'auto', fontSize: '0.7rem' }}>
              {builtCbor}
            </pre>
            <button
              className="btn btn-secondary"
              style={{ position: 'absolute', top: '0.5rem', right: '0.5rem', padding: '0.2rem 0.6rem', fontSize: '0.75rem' }}
              onClick={handleCopy}
            >
              {copied ? 'Copied!' : 'Copy'}
            </button>
          </div>
          <pre className="code-block" style={{ marginTop: '0.75rem', fontSize: '0.78rem' }}>{`# Save as tx envelope
echo '{"type":"Tx ConwayEra","description":"","cborHex":"<paste above>"}' > tx.raw

# Sign
cardano-cli transaction sign \\
  --tx-file tx.raw \\
  --signing-key-file <actor>.sk \\
  --out-file tx.signed

# Get signed CBOR
cardano-cli transaction view --tx-file tx.signed | jq -r .cborHex`}</pre>

          <p className="register-desc" style={{ margin: '1rem 0 0.5rem' }}>
            <strong>Step 2 — Submit the signed transaction</strong>
          </p>
          <div className="form-group" style={{ marginBottom: '0.75rem' }}>
            <label>Paste signed CBOR hex</label>
            <textarea
              rows={3}
              placeholder="84a500..."
              value={signedCbor}
              onChange={e => setSignedCbor(e.target.value)}
              style={{ fontFamily: 'var(--font-mono)', fontSize: '0.78rem', resize: 'vertical' }}
            />
          </div>

          {err && <div className="register-result error" style={{ marginBottom: '0.75rem' }}><p>{err}</p></div>}

          <div style={{ display: 'flex', gap: '0.75rem' }}>
            <button
              className="btn btn-primary"
              onClick={handleSubmit}
              disabled={submitting || !signedCbor.trim()}
            >
              {submitting ? 'Submitting…' : 'Submit to Head'}
            </button>
            <button className="btn btn-secondary" onClick={() => setBuiltCbor(null)}>
              Back
            </button>
          </div>
        </>
      )}
    </div>
  )
}

// ─── HopCard ──────────────────────────────────────────────────────────────

interface HopCardProps {
  hop: HopStatusResponse
  action: ParticipantAction | undefined
  routeId: string
  onDone: () => void
}

function HopCard({ hop, action, routeId, onDone }: HopCardProps) {
  const [expanded, setExpanded] = useState(false)

  const actionLabel =
    action?.kind === 'lock' ? '⚡ Action needed: Lock HTLC' :
    action?.kind === 'claim' ? '⚡ Action needed: Claim HTLC' :
    action?.kind === 'refund' ? '⚠ Action needed: Refund HTLC' : null

  return (
    <div className="hop-timeline-card glow-card" style={{ marginBottom: '0.75rem' }}>
      <div className="hop-timeline-header" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <span className="hop-timeline-index">Hop {hop.hopIndex + 1}</span>
        <div style={{ display: 'flex', gap: '0.75rem', alignItems: 'center' }}>
          {action && (
            <span style={{ fontSize: '0.75rem', color: urgencyColor[action.urgency] ?? 'var(--text-muted)', fontFamily: 'var(--font-mono)' }}>
              {action.urgency === 'expiring' ? '⚠ expiring soon' : action.urgency === 'expired' ? '✗ expired' : action.urgency === 'soon' ? '⏱ act soon' : ''}
            </span>
          )}
          <span className="hop-timeline-status" style={{ color: hopStatusColor[hop.htlcStatus] ?? 'var(--text-muted)' }}>
            {hop.htlcStatus}
          </span>
        </div>
      </div>

      <div className="hop-timeline-body">
        <div className="meta-row"><span className="meta-label">Head</span><code className="meta-value">{hop.headId.slice(0, 14)}…</code></div>
        <div className="meta-row"><span className="meta-label">Sender</span><code className="meta-value">{hop.senderAddress.slice(0, 16)}…</code></div>
        <div className="meta-row"><span className="meta-label">Receiver</span><code className="meta-value">{hop.receiverAddress.slice(0, 16)}…</code></div>
        <div className="meta-row"><span className="meta-label">Fee</span><span className="meta-value">{(hop.fee / 1_000_000).toFixed(6)} ADA</span></div>
        <div className="meta-row"><span className="meta-label">Timeout slot</span><span className="meta-value">{hop.timeoutSlot}</span></div>
        {hop.htlcTxHash && <div className="meta-row"><span className="meta-label">Lock tx</span><code className="meta-value">{hop.htlcTxHash.slice(0, 16)}…</code></div>}
      </div>

      {action && (
        <div style={{ marginTop: '0.75rem' }}>
          <button
            className={`btn ${action.urgency === 'expiring' || action.urgency === 'expired' ? 'btn-primary' : 'btn-secondary'}`}
            style={{ fontSize: '0.85rem' }}
            onClick={() => setExpanded(v => !v)}
          >
            {actionLabel} {expanded ? '▲' : '▼'}
          </button>
          <AnimatePresence>
            {expanded && (
              <motion.div
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: 'auto' }}
                exit={{ opacity: 0, height: 0 }}
                transition={{ duration: 0.25 }}
                style={{ overflow: 'hidden' }}
              >
                <TxPanel
                  action={action}
                  routeId={routeId}
                  headId={hop.headId}
                  secretHash={hop.secretHash}
                  onDone={() => { setExpanded(false); onDone() }}
                />
              </motion.div>
            )}
          </AnimatePresence>
        </div>
      )}

      {!action && hop.htlcStatus === 'pending' && (
        <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginTop: '0.5rem', fontStyle: 'italic' }}>
          Waiting for the previous hop to be locked first.
        </p>
      )}
    </div>
  )
}

// ─── RouteCard ────────────────────────────────────────────────────────────

interface RouteCardProps {
  summary: ParticipantRouteSummary
  onRefresh: () => void
}

function RouteCard({ summary, onRefresh }: RouteCardProps) {
  const { route, roles, actions } = summary
  const [preimageInput, setPreimageInput] = useState('')
  const [revealing, setRevealing] = useState(false)
  const [revealOk, setRevealOk] = useState(false)
  const [revealErr, setRevealErr] = useState<string | null>(null)

  const lastHop = route.hops[route.hops.length - 1]
  const isReceiver = roles.includes('receiver')
  const hasLockedHop = route.hops.some(h => h.htlcStatus === 'locked')
  const showPreimageReveal = isReceiver && hasLockedHop && route.status === 'in_progress'

  const handleReveal = async () => {
    setRevealErr(null)
    setRevealing(true)
    try {
      await submitPreimage(lastHop.secretHash, preimageInput.trim())
      setRevealOk(true)
      setTimeout(onRefresh, 1500)
    } catch (e) {
      setRevealErr(e instanceof Error ? e.message : 'Failed to reveal preimage')
    } finally {
      setRevealing(false)
    }
  }

  const statusColor =
    route.status === 'completed' ? 'var(--success)' :
    route.status === 'failed' ? 'var(--error)' :
    route.status === 'in_progress' ? '#f0c040' :
    'var(--text-muted)'

  return (
    <motion.div
      className="glow-card"
      style={{ marginBottom: '1.5rem', padding: '1.25rem' }}
      initial={{ opacity: 0, y: 15 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.35 }}
    >
      {/* Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '0.75rem', flexWrap: 'wrap', gap: '0.5rem' }}>
        <div>
          <code style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>{route.routeId}</code>
          <div style={{ marginTop: '0.4rem' }}>{roles.map(roleBadge)}</div>
        </div>
        <div style={{ textAlign: 'right' }}>
          <span style={{ color: statusColor, fontFamily: 'var(--font-mono)', fontSize: '0.85rem' }}>
            {route.status.toUpperCase()}
          </span>
          <div style={{ fontSize: '0.8rem', color: 'var(--text-dim)', marginTop: '0.25rem' }}>
            {(route.amountLovelace / 1_000_000).toFixed(2)} ADA · fee {(route.totalFee / 1_000_000).toFixed(4)} ADA
          </div>
        </div>
      </div>

      {/* Preimage reveal for receivers */}
      {showPreimageReveal && !revealOk && (
        <div className="register-result" style={{ marginBottom: '1rem', borderColor: '#f0c040', background: 'rgba(240,192,64,0.05)' }}>
          <p style={{ margin: '0 0 0.75rem', color: '#f0c040', fontWeight: 600 }}>
            A hop is locked and waiting — reveal your secret to trigger the claim cascade
          </p>
          <div className="form-group" style={{ marginBottom: '0.5rem' }}>
            <input
              type="text"
              placeholder="Your secret (hex) — from openssl rand -hex 32"
              value={preimageInput}
              onChange={e => setPreimageInput(e.target.value)}
            />
          </div>
          {revealErr && <p style={{ color: 'var(--error)', fontSize: '0.85rem', margin: '0.25rem 0' }}>{revealErr}</p>}
          <button className="btn btn-primary" onClick={handleReveal} disabled={revealing || !preimageInput.trim()}>
            {revealing ? 'Revealing…' : 'Reveal Secret & Notify Bridges'}
          </button>
        </div>
      )}
      {revealOk && (
        <div className="register-result success" style={{ marginBottom: '1rem' }}>
          <p>Secret revealed. Bridge operators have been notified. Refreshing…</p>
        </div>
      )}

      {/* Hop list */}
      <div style={{ marginTop: '0.5rem' }}>
        {route.hops.map(hop => {
          const action = actions.find(a => a.hopIndex === hop.hopIndex)
          return (
            <HopCard
              key={hop.hopIndex}
              hop={hop}
              action={action}
              routeId={route.routeId}
              onDone={onRefresh}
            />
          )
        })}
      </div>

      <div style={{ marginTop: '0.75rem', textAlign: 'right' }}>
        <Link
          to={`/payments/${route.routeId}`}
          style={{ fontSize: '0.8rem', color: 'var(--text-muted)', textDecoration: 'underline' }}
        >
          Full payment tracker →
        </Link>
      </div>
    </motion.div>
  )
}

// ─── Dashboard ────────────────────────────────────────────────────────────

export default function Dashboard() {
  const { address: walletAddress } = useWallet()
  const [keyHash, setKeyHash] = useState<string>('')
  const [input, setInput] = useState<string>('')
  const [loading, setLoading] = useState(false)
  const [summaries, setSummaries] = useState<ParticipantRouteSummary[] | null>(null)
  const [error, setError] = useState<string | null>(null)

  // Load saved key hash from backend when wallet connects
  useEffect(() => {
    if (!walletAddress) { setKeyHash(''); setInput(''); return }
    getUserKeyHash(walletAddress)
      .then(r => { if (r.keyHash) { setKeyHash(r.keyHash); setInput(r.keyHash) } })
      .catch(() => {})
  }, [walletAddress])

  const load = useCallback(async (pkh: string) => {
    setError(null)
    setLoading(true)
    try {
      const data = await getParticipantRoutes(pkh)
      setSummaries(data)
    } catch (e) {
      setError(e instanceof Error ? e.message : 'Failed to load dashboard')
      setSummaries(null)
    } finally {
      setLoading(false)
    }
  }, [])

  const handleLoad = (e: React.FormEvent) => {
    e.preventDefault()
    const pkh = input.trim()
    if (!pkh.match(/^[0-9a-fA-F]{56}$/)) {
      setError('Key hash must be a 56-character hex string.')
      return
    }
    setKeyHash(pkh)
    if (walletAddress) setUserKeyHash(walletAddress, pkh).catch(() => {})
    load(pkh)
  }

  const refresh = () => { if (keyHash) load(keyHash) }

  const active = summaries?.filter(s => s.route.status === 'in_progress' || s.route.status === 'requested') ?? []
  const done = summaries?.filter(s => s.route.status === 'completed' || s.route.status === 'failed') ?? []

  return (
    <div className="payment-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Dashboard</h1>
        <p className="register-desc">
          Enter your Cardano key hash to see all your active payments and take action on each hop —
          lock funds, claim with your secret, or refund after a timeout.
        </p>

        <form className="register-form" onSubmit={handleLoad}>
          <div className="form-group">
            <label htmlFor="pkh">Your Cardano key hash</label>
            <input
              id="pkh"
              type="text"
              placeholder="56-character hex"
              value={input}
              onChange={e => setInput(e.target.value)}
            />
            <span className="form-hint">
              Derive with:{' '}
              <code>cardano-cli address key-hash --payment-verification-key-file &lt;actor&gt;.vk</code>
              {' '}· <Link to="/setup" style={{ color: 'var(--text-muted)', textDecoration: 'underline' }}>Setup guide</Link>
            </span>
          </div>
          {error && !loading && (
            <div className="register-result error"><p>{error}</p></div>
          )}
          <button type="submit" className="btn btn-primary btn-full" disabled={loading}>
            {loading ? 'Loading…' : 'Load Dashboard'}
          </button>
        </form>
      </motion.section>

      {summaries !== null && (
        <motion.section
          className="section"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.4 }}
        >
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '1.5rem' }}>
            <h2 className="section-title" style={{ margin: 0 }}>
              Active Payments {active.length > 0 && <span style={{ color: '#f0c040' }}>({active.length})</span>}
            </h2>
            <button className="btn btn-secondary" style={{ fontSize: '0.8rem' }} onClick={refresh} disabled={loading}>
              {loading ? 'Refreshing…' : '↻ Refresh'}
            </button>
          </div>

          {active.length === 0 && (
            <div className="register-result" style={{ textAlign: 'center' }}>
              <p>No active payments. <Link to="/routes" style={{ color: 'var(--text-muted)', textDecoration: 'underline' }}>Send a payment</Link> or <Link to="/invoice" style={{ color: 'var(--text-muted)', textDecoration: 'underline' }}>create an invoice</Link>.</p>
            </div>
          )}

          {active.map(s => (
            <RouteCard key={s.route.routeId} summary={s} onRefresh={refresh} />
          ))}

          {done.length > 0 && (
            <>
              <h2 className="section-title" style={{ marginTop: '2.5rem' }}>Completed</h2>
              {done.map(s => (
                <RouteCard key={s.route.routeId} summary={s} onRefresh={refresh} />
              ))}
            </>
          )}
        </motion.section>
      )}
    </div>
  )
}
