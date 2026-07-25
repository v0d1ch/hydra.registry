import { useState, useCallback, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'
import {
  getHeads, getRegisteredHead, getHeadParticipants, getUserKeyHash,
  getPendingInvoices, claimOwnership,
  type RegisteredHeadDetail, type InvoiceResponse,
} from '../api/client'
import { useWallet } from '../context/WalletContext'
import { useUser } from '../context/UserContext'

const HTLC_SCRIPT_HASH = '81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df'
const HTLC_REPO = 'https://github.com/v0d1ch/htlc'

type ClaimStatus = 'idle' | 'loading' | 'success' | 'error'

export default function Register() {
  const { address: walletAddress } = useWallet()
  const { pendingInvoice: contextInvoice } = useUser()

  const [registeredHeads, setRegisteredHeads] = useState<RegisteredHeadDetail[]>([])
  const [headsLoading, setHeadsLoading] = useState(true)
  const [myHeadIds, setMyHeadIds] = useState<Set<string>>(new Set())
  const [userKeyHash, setUserKeyHash] = useState<string | null>(null)
  const [pendingInvoices, setPendingInvoices] = useState<InvoiceResponse[]>([])

  const [claimStatus, setClaimStatus] = useState<Record<string, ClaimStatus>>({})
  const [claimError, setClaimError] = useState<Record<string, string>>({})
  const [claimKeyHash, setClaimKeyHash] = useState<Record<string, string>>({})

  const [aboutOpen, setAboutOpen] = useState(false)

  const loadHeads = useCallback(async () => {
    setHeadsLoading(true)
    try {
      const heads = await getHeads()
      const details = await Promise.all(heads.map(h => getRegisteredHead(h.headId).catch(() => null)))
      setRegisteredHeads(details.filter((d): d is RegisteredHeadDetail => d !== null))
    } catch {
      setRegisteredHeads([])
    } finally {
      setHeadsLoading(false)
    }
  }, [])

  useEffect(() => { loadHeads() }, [loadHeads])

  // Auto-refresh every 15 s so newly registered heads appear
  useEffect(() => {
    const id = setInterval(loadHeads, 15_000)
    return () => clearInterval(id)
  }, [loadHeads])

  useEffect(() => {
    if (!walletAddress) { setUserKeyHash(null); return }
    getUserKeyHash(walletAddress)
      .then(r => setUserKeyHash(r.keyHash))
      .catch(() => setUserKeyHash(null))
  }, [walletAddress])

  useEffect(() => {
    if (!userKeyHash || registeredHeads.length === 0) { setMyHeadIds(new Set()); return }
    Promise.all(
      registeredHeads.map(h =>
        getHeadParticipants(h.headId)
          .then(ps => ps.some(p => p.onChainId === userKeyHash) ? h.headId : null)
          .catch(() => null)
      )
    ).then(results => {
      setMyHeadIds(new Set(results.filter((id): id is string => id !== null)))
    })
  }, [userKeyHash, registeredHeads])

  useEffect(() => {
    getPendingInvoices()
      .then(invs => {
        if (contextInvoice && contextInvoice.status === 'pending' && !invs.some(i => i.invoiceId === contextInvoice.invoiceId)) {
          setPendingInvoices([contextInvoice, ...invs])
        } else {
          setPendingInvoices(invs)
        }
      })
      .catch(() => {
        if (contextInvoice && contextInvoice.status === 'pending') setPendingInvoices([contextInvoice])
      })
  }, [contextInvoice])

  const handleClaimOwnership = async (headId: string) => {
    if (!walletAddress) return
    setClaimStatus(s => ({ ...s, [headId]: 'loading' }))
    setClaimError(e => { const n = { ...e }; delete n[headId]; return n })
    try {
      const res = await claimOwnership(headId, walletAddress)
      setClaimKeyHash(k => ({ ...k, [headId]: res.keyHash }))
      setClaimStatus(s => ({ ...s, [headId]: 'success' }))
      setUserKeyHash(res.keyHash)
      await loadHeads()
    } catch (err) {
      setClaimStatus(s => ({ ...s, [headId]: 'error' }))
      setClaimError(e => ({ ...e, [headId]: err instanceof Error ? err.message : 'Claim failed' }))
    }
  }

  const registryUrl = window.location.origin

  return (
    <div className="register-page">

      {/* ── Registered heads ── */}
      {!headsLoading && registeredHeads.length > 0 && (
        <motion.div
          className="prerequisite-card glow-card"
          style={{ marginBottom: '2rem' }}
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.4 }}
        >
          <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', marginBottom: '0.75rem', fontWeight: 600 }}>
            {registeredHeads.length} head{registeredHeads.length > 1 ? 's' : ''} registered
          </p>
          <div style={{ display: 'flex', flexDirection: 'column', gap: '1rem' }}>
            {registeredHeads.map(h => {
              const isMine = myHeadIds.has(h.headId)
              const htlcEnabled = h.htlcEnabled ?? false
              const invoices = pendingInvoices.filter(i => i.headId === h.headId)
              const cs = claimStatus[h.headId] ?? 'idle'
              const showClaimBtn = walletAddress && !isMine && (claimKeyHash[h.headId] === undefined)
              return (
                <div key={h.headId} style={{ borderTop: '1px solid var(--border)', paddingTop: '0.75rem' }}>
                  <code style={{ fontSize: '0.8rem', color: isMine ? 'var(--success)' : 'var(--accent)', wordBreak: 'break-all', display: 'block', marginBottom: '0.2rem' }}>{h.headId}</code>
                  <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>
                    {h.status}
                    {isMine && <span style={{ color: 'var(--success)', marginLeft: '0.5rem' }}>← you</span>}
                  </span>

                  {/* Claim ownership */}
                  {showClaimBtn && (
                    <div style={{ marginTop: '0.75rem', padding: '0.75rem', background: 'var(--surface)', borderRadius: '6px', border: '1px solid var(--border)' }}>
                      <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.5rem', fontWeight: 600 }}>Claim ownership of this head</p>
                      <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: '0.5rem', lineHeight: 1.6 }}>
                        To prove you control this head, deposit a UTxO from your connected wallet into it.
                        Run this <strong style={{ color: 'var(--text)' }}>on the machine where your hydra-node runs</strong> — it
                        talks to your node's local API; the registry is not involved in this step:
                      </p>
                      <pre style={{ fontSize: '0.75rem', background: 'var(--bg)', border: '1px solid var(--border)', borderRadius: '4px', padding: '0.5rem 0.75rem', overflowX: 'auto', marginBottom: '0.5rem', color: 'var(--text)' }}>{`curl http://127.0.0.1:4001/commit \\
  -H 'Content-Type: application/json' \\
  -d '{"utxo": {}}'`}</pre>
                      <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: '0.75rem', lineHeight: 1.6 }}>
                        Sign the returned CBOR with your wallet and submit it to L1. Your agent streams the
                        confirming snapshot to the registry; once your wallet address shows up in the head's
                        UTxO set here, click below to verify.
                      </p>
                      <button
                        type="button"
                        className="btn btn-secondary"
                        style={{ fontSize: '0.8rem', padding: '0.3rem 0.75rem' }}
                        onClick={() => handleClaimOwnership(h.headId)}
                        disabled={cs === 'loading'}
                      >
                        {cs === 'loading' ? 'Verifying…' : 'Claim ownership'}
                      </button>
                      {cs === 'error' && (
                        <p style={{ marginTop: '0.4rem', fontSize: '0.78rem', color: 'var(--error)' }}>
                          {claimError[h.headId]}
                        </p>
                      )}
                    </div>
                  )}
                  {cs === 'success' && claimKeyHash[h.headId] && (
                    <p style={{ marginTop: '0.4rem', fontSize: '0.78rem', color: 'var(--success)' }}>
                      Ownership verified · key hash: <code style={{ wordBreak: 'break-all' }}>{claimKeyHash[h.headId]}</code>
                    </p>
                  )}

                  {/* HTLC / invoice actions */}
                  <div style={{ marginTop: '0.6rem', fontSize: '0.85rem' }}>
                    {!htlcEnabled ? (
                      <p style={{ color: 'var(--text-muted)', margin: 0 }}>
                        <strong style={{ color: 'var(--text)' }}>Next:</strong> Publish the HTLC validator — see <Link to="/setup" style={{ color: 'var(--accent)' }}>Setup guide → step 04</Link>
                      </p>
                    ) : invoices.length > 0 ? (
                      <div>
                        {invoices.map(inv => (
                          <div key={inv.invoiceId} style={{ marginBottom: '0.4rem', padding: '0.5rem 0.75rem', background: 'rgba(0,212,170,0.06)', borderRadius: '6px', border: '1px solid rgba(0,212,170,0.2)' }}>
                            <p style={{ margin: 0, color: 'var(--success)', fontWeight: 600 }}>Waiting for payment</p>
                            <code style={{ fontSize: '0.75rem', color: 'var(--text-muted)', wordBreak: 'break-all' }}>{inv.invoiceId}</code>
                            <span style={{ display: 'block', fontSize: '0.75rem', color: 'var(--text-muted)', marginTop: '0.2rem' }}>
                              {(inv.amountLovelace / 1_000_000).toFixed(2)} ADA · expires {new Date(inv.expiresAt).toLocaleString()}
                            </span>
                          </div>
                        ))}
                        <Link to="/invoice" style={{ fontSize: '0.8rem', color: 'var(--accent)' }}>+ Create another invoice</Link>
                      </div>
                    ) : (
                      <div style={{ display: 'flex', gap: '0.5rem', flexWrap: 'wrap' }}>
                        <Link to="/invoice" className="btn btn-primary" style={{ fontSize: '0.8rem', padding: '0.3rem 0.75rem' }}>Receive a payment</Link>
                        <Link to="/routes" className="btn btn-secondary" style={{ fontSize: '0.8rem', padding: '0.3rem 0.75rem' }}>Send a payment</Link>
                      </div>
                    )}
                  </div>
                </div>
              )
            })}
          </div>
          {!walletAddress && (
            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginTop: '0.75rem' }}>
              Connect your wallet to see which heads you're in or to claim ownership.
            </p>
          )}
        </motion.div>
      )}

      {headsLoading && (
        <motion.div
          className="prerequisite-card glow-card"
          style={{ marginBottom: '2rem' }}
          initial={{ opacity: 0 }} animate={{ opacity: 1 }}
        >
          <p style={{ color: 'var(--text-muted)', fontSize: '0.85rem' }}>Loading heads…</p>
        </motion.div>
      )}

      {/* ── Agent instructions ── */}
      <motion.div
        className="prerequisite-card glow-card"
        style={{ marginBottom: '2rem' }}
        initial={{ opacity: 0, y: 10 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.4, delay: 0.1 }}
      >
        <h2 style={{ fontSize: '1rem', fontWeight: 700, marginBottom: '0.75rem' }}>Register a head</h2>
        <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', marginBottom: '0.75rem', lineHeight: 1.6 }}>
          Heads are registered automatically by the <strong style={{ color: 'var(--text)' }}>hydra-registry-agent</strong>,
          which runs on the same machine as your hydra-node. It reads events from the node's{' '}
          <em>local</em> WebSocket and pushes them here, publishes your node's protocol parameters,
          and relays the transactions you sign on this site to your node for submission — all over
          outbound connections only.
        </p>
        <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', marginBottom: '1rem', lineHeight: 1.6 }}>
          Your hydra-node's API is unauthenticated, so it must never be reachable from the internet.
          With the agent, it never has to be — this registry cannot and does not connect to your node.
        </p>

        <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.4rem', fontWeight: 600 }}>1. Build the agent</p>
        <pre style={{
          background: 'var(--surface)', border: '1px solid var(--border)', borderRadius: '6px',
          padding: '0.75rem 1rem', fontSize: '0.78rem', overflowX: 'auto', marginBottom: '1rem',
          color: 'var(--text)',
        }}>{`git clone https://github.com/v0d1ch/hydra.registry
cd hydra.registry
nix develop --command bash -c "cd api && cabal build exe:hydra-registry-agent"`}</pre>

        <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.4rem', fontWeight: 600 }}>2. Point it at your node and this registry</p>
        <pre style={{
          background: 'var(--surface)', border: '1px solid var(--border)', borderRadius: '6px',
          padding: '0.75rem 1rem', fontSize: '0.78rem', overflowX: 'auto', marginBottom: '1rem',
          color: 'var(--text)',
        }}>{`export HYDRA_NODE_WS_URL=ws://127.0.0.1:4001
export HYDRA_REGISTRY_URL=${registryUrl}
export HYDRA_AGENT_STATE_FILE=$HOME/.hydra-agent-state.json`}</pre>

        <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.4rem', fontWeight: 600 }}>3. Run it — and keep it running</p>
        <pre style={{
          background: 'var(--surface)', border: '1px solid var(--border)', borderRadius: '6px',
          padding: '0.75rem 1rem', fontSize: '0.78rem', overflowX: 'auto', marginBottom: '1rem',
          color: 'var(--text)',
        }}>{`cabal run exe:hydra-registry-agent`}</pre>

        <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
          The agent registers itself on first run (credentials are stored in the state file — keep it).
          Once your head is <code>Open</code> it appears above; connect your wallet and click{' '}
          <strong style={{ color: 'var(--text)' }}>Claim ownership</strong> to link your key hash to it.
          If the agent stops, payment actions on this site fail with <em>"no live agent"</em> until it's back.
          Full walkthrough in the <Link to="/setup" style={{ color: 'var(--accent)' }}>Setup guide → step 03</Link>.
        </p>
      </motion.div>

      {/* ── About HTLC ── */}
      <motion.section className="section" initial={{ opacity: 0, y: 20 }} animate={{ opacity: 1, y: 0 }} transition={{ delay: 0.3, duration: 0.5 }}>
        <button type="button" className="about-htlc-toggle" onClick={() => setAboutOpen(!aboutOpen)}>
          <span className="about-htlc-toggle-icon">{aboutOpen ? '▼' : '▶'}</span>
          About the HTLC Contract
        </button>
        <AnimatePresence>
          {aboutOpen && (
            <motion.div initial={{ opacity: 0, height: 0 }} animate={{ opacity: 1, height: 'auto' }}
              exit={{ opacity: 0, height: 0 }} transition={{ duration: 0.3 }} style={{ overflow: 'hidden' }}
            >
              <div className="prerequisite-card glow-card">
                <h3>HTLC Contract</h3>
                <p className="register-desc" style={{ marginTop: '0.75rem' }}>
                  Registering a head as HTLC-capable is a policy declaration: bridge operators commit to
                  constructing lock and claim transactions at payment time. There is no on-chain HTLC
                  state until a payment flows through the bridge.
                </p>
                <div className="result-details">
                  <div className="result-row"><span className="result-label">Script hash</span><code className="result-value result-value-mono">{HTLC_SCRIPT_HASH}</code></div>
                  <div className="result-row"><span className="result-label">Source</span><a href={HTLC_REPO} className="result-value" target="_blank" rel="noopener noreferrer">{HTLC_REPO}</a></div>
                  <div className="result-row"><span className="result-label">Plutus version</span><span className="result-value">V3 (Aiken)</span></div>
                </div>
              </div>
            </motion.div>
          )}
        </AnimatePresence>
      </motion.section>
    </div>
  )
}
