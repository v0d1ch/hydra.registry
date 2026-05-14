import { useState, useCallback, useEffect } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'
import { registerHead, checkHead, getHeadParticipants } from '../api/client'

// Persist the in-progress wizard across navigation.
const WIZARD_STORAGE_KEY = 'registerWizard'

interface WizardState {
  step: number
  selectedNetwork: string
  host: string
  port: string
}

function loadWizardState(): Partial<WizardState> {
  try {
    const raw = localStorage.getItem(WIZARD_STORAGE_KEY)
    if (!raw) return {}
    const parsed = JSON.parse(raw)
    return typeof parsed === 'object' && parsed !== null ? parsed : {}
  } catch {
    return {}
  }
}

const HTLC_SCRIPT_HASH = '81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df'
const HTLC_REPO = 'https://github.com/v0d1ch/htlc'

const NETWORKS = ['Mainnet', 'Preview', 'Preprod'] as const

const STEPS = ['Connection', 'Register']

function loadRegisteredHeads(): { headId: string; host: string; port: number }[] {
  try {
    const raw = localStorage.getItem('registeredHeads')
    if (!raw) return []
    const parsed = JSON.parse(raw)
    if (!Array.isArray(parsed)) return []
    // Deduplicate by headId, keeping the last entry for each
    const seen = new Map<string, { headId: string; host: string; port: number }>()
    for (const h of parsed) {
      if (h?.headId) seen.set(h.headId, h)
    }
    const deduped = Array.from(seen.values())
    localStorage.setItem('registeredHeads', JSON.stringify(deduped))
    return deduped
  } catch {
    return []
  }
}

export default function Register() {
  const location = useLocation()
  const saved = loadWizardState()
  const [step, setStep] = useState(saved.step ?? 0)
  const [selectedNetwork, setSelectedNetwork] = useState<string>(saved.selectedNetwork ?? NETWORKS[1])
  const [host, setHost] = useState(saved.host ?? '')
  const [port, setPort] = useState(saved.port ?? '')
  const [registeredHeads, setRegisteredHeads] = useState(loadRegisteredHeads)
  const userKeyHash = localStorage.getItem('userKeyHash') ?? ''
  const [myHeadIds, setMyHeadIds] = useState<Set<string>>(new Set())

  useEffect(() => {
    if (!userKeyHash || registeredHeads.length === 0) return
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

  // Connection step error
  const [connectError, setConnectError] = useState<string | null>(null)

  // Registration state
  const [regLoading, setRegLoading] = useState(false)
  const [regResult, setRegResult] = useState<{ headId: string; status: string } | null>(null)
  const [regError, setRegError] = useState<string | null>(null)

  // About section
  const [aboutOpen, setAboutOpen] = useState(false)

  useEffect(() => {
    const data: WizardState = {
      step, selectedNetwork, host, port,
    }
    try { localStorage.setItem(WIZARD_STORAGE_KEY, JSON.stringify(data)) } catch { /* ignore */ }
  }, [step, selectedNetwork, host, port])

  useEffect(() => {
    if (regResult) {
      try { localStorage.removeItem(WIZARD_STORAGE_KEY) } catch { /* ignore */ }
    }
  }, [regResult])

  // Clicking the navbar "Register" link while already on /register doesn't
  // change the URL, but React Router still issues a fresh `location.key`
  // for the same-path navigation. If we're showing a success card from a
  // previous registration when that fires, reset the form so the next
  // click feels like a fresh start.
  useEffect(() => {
    if (regResult) {
      setStep(0)
      setHost('')
      setPort('')
      setConnectError(null)
      setRegResult(null)
      setRegError(null)
    }
    // intentionally only on location.key change
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [location.key])

  const resetWizard = () => {
    try { localStorage.removeItem(WIZARD_STORAGE_KEY) } catch { /* ignore */ }
    setStep(0)
    setHost('')
    setPort('')
    setConnectError(null)
    setRegResult(null)
    setRegError(null)
  }

  const validateStep1 = useCallback((): string | null => {
    if (!host.trim()) return 'Host is required'
    const portNum = parseInt(port, 10)
    if (isNaN(portNum) || portNum < 1 || portNum > 65535) return 'Port must be a number between 1 and 65535'
    return null
  }, [host, port])

  const [checkLoading, setCheckLoading] = useState(false)

  const handleNextStep1 = async () => {
    const err = validateStep1()
    if (err) {
      setConnectError(err)
      return
    }
    setConnectError(null)
    setCheckLoading(true)

    try {
      const portNum = parseInt(port, 10)
      const res = await checkHead(host, portNum)
      if (res.alreadyRegistered) {
        setConnectError(`Head ${res.headId} is already registered.`)
        return
      }
      setStep(1)
    } catch (e) {
      setConnectError(e instanceof Error ? e.message : 'Failed to connect to Hydra node')
    } finally {
      setCheckLoading(false)
    }
  }

  const handleRegister = async () => {
    setRegLoading(true)
    setRegError(null)
    setRegResult(null)

    const portNum = parseInt(port, 10)

    try {
      const res = await registerHead(host, portNum)
      setRegResult(res)
      const stored = JSON.parse(localStorage.getItem('registeredHeads') ?? '[]')
      stored.push({ headId: res.headId, host, port: portNum, registeredAt: new Date().toISOString() })
      localStorage.setItem('registeredHeads', JSON.stringify(stored))
      setRegisteredHeads(loadRegisteredHeads())
    } catch (err) {
      setRegError(err instanceof Error ? err.message : 'Registration failed')
    } finally {
      setRegLoading(false)
    }
  }

  return (
    <div className="register-page">
      {registeredHeads.length > 0 && !regResult && (
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
          <div style={{ display: 'flex', flexDirection: 'column', gap: '0.4rem', marginBottom: '1rem' }}>
            {registeredHeads.map(h => {
              const isMine = myHeadIds.has(h.headId)
              return (
                <div key={h.headId} style={{ fontSize: '0.8rem', color: 'var(--text-muted)', display: 'flex', gap: '0.5rem', alignItems: 'center' }}>
                  <code style={{ color: isMine ? 'var(--success)' : 'var(--accent)' }}>{h.headId.slice(0, 16)}…</code>
                  <span>{h.host}:{h.port}</span>
                  {isMine && <span style={{ color: 'var(--success)', fontSize: '0.75rem' }}>← you</span>}
                </div>
              )
            })}
          </div>
          {userKeyHash && myHeadIds.size === 0 && (
            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
              Your key hash is not a participant in any of these heads.
            </p>
          )}
          {!userKeyHash && (
            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
              Set your key hash on the <Link to="/dashboard" style={{ color: 'var(--accent)' }}>Dashboard</Link> to see which heads you're in.
            </p>
          )}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '0.4rem', fontSize: '0.9rem', color: 'var(--text-muted)', borderTop: '1px solid var(--border)', paddingTop: '0.75rem' }}>
            <p><strong style={{ color: 'var(--text)' }}>1. Publish the HTLC validator</strong> — see <Link to="/setup" style={{ color: 'var(--accent)' }}>Setup guide → step 04</Link></p>
            <p><strong style={{ color: 'var(--text)' }}>2. Receive a payment</strong> — <Link to="/invoice" style={{ color: 'var(--accent)' }}>create an invoice</Link></p>
            <p><strong style={{ color: 'var(--text)' }}>3. Send a payment</strong> — <Link to="/routes" style={{ color: 'var(--accent)' }}>find a route</Link></p>
          </div>
        </motion.div>
      )}

      {step > 0 && (
        <div style={{ display: 'flex', justifyContent: 'flex-end', marginBottom: '0.5rem' }}>
          <button type="button" className="btn btn-secondary" onClick={resetWizard}>
            Start over
          </button>
        </div>
      )}
      {/* Progress bar */}
      <motion.div
        className="wizard-progress"
        initial={{ opacity: 0, y: -10 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.4 }}
      >
        {STEPS.map((label, i) => (
          <button
            key={label}
            className={`wizard-step ${i === step ? 'wizard-step-active' : ''} ${i < step ? 'wizard-step-done' : ''}`}
            onClick={() => { if (i < step) setStep(i) }}
            disabled={i > step}
          >
            <span className="wizard-step-num">{i < step ? '✓' : i + 1}</span>
            <span className="wizard-step-label">{label}</span>
          </button>
        ))}
        <div className="wizard-progress-bar">
          <div className="wizard-progress-fill" style={{ width: `${(step / (STEPS.length - 1)) * 100}%` }} />
        </div>
      </motion.div>

      <AnimatePresence mode="wait">
        {/* ─── Step 1: Connection ─── */}
        {step === 0 && (
          <motion.section
            key="step-0"
            className="section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: 20 }}
            transition={{ duration: 0.3 }}
          >
            <h1 className="section-title">Head Connection</h1>
            <p className="register-desc">
              Enter your Hydra node connection details and network.
            </p>

            <div className="register-form">
              <div className="form-group">
                <label htmlFor="host">Host</label>
                <input
                  id="host"
                  type="text"
                  placeholder="e.g. 192.168.1.100 or my-hydra-node.example.com"
                  value={host}
                  onChange={(e) => setHost(e.target.value)}
                />
              </div>
              <div className="form-group">
                <label htmlFor="port">Port</label>
                <input
                  id="port"
                  type="text"
                  placeholder="e.g. 4001"
                  value={port}
                  onChange={(e) => setPort(e.target.value)}
                />
              </div>

              <div className="form-group">
                <label>Network</label>
                <div className="network-selector register-network-selector" style={{ marginTop: 0 }}>
                  {NETWORKS.map(n => (
                    <button
                      key={n}
                      type="button"
                      className={`network-btn ${selectedNetwork === n ? 'network-active' : ''}`}
                      onClick={() => setSelectedNetwork(n)}
                    >
                      {n}
                    </button>
                  ))}
                </div>
              </div>

              {connectError && (
                <div className="register-result error" style={{ marginBottom: '1rem' }}>
                  <p>{connectError}</p>
                </div>
              )}

              <button type="button" className="btn btn-primary btn-full" onClick={handleNextStep1} disabled={checkLoading}>
                {checkLoading ? 'Checking...' : 'Continue'}
              </button>
            </div>
          </motion.section>
        )}

        {/* ─── Step 2: Register ─── */}
        {step === 1 && (
          <motion.section
            key="step-1"
            className="section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: 20 }}
            transition={{ duration: 0.3 }}
          >
            <h1 className="section-title">Register Head</h1>
            <p className="register-desc">
              Register your head at <code>{host}:{port}</code> on <strong>{selectedNetwork}</strong>.
            </p>

            <div className="register-form">
              <div className="result-details" style={{ marginBottom: '1.5rem' }}>
                <div className="result-row">
                  <span className="result-label">Host</span>
                  <span className="result-value">{host}</span>
                </div>
                <div className="result-row">
                  <span className="result-label">Port</span>
                  <span className="result-value">{port}</span>
                </div>
                <div className="result-row">
                  <span className="result-label">Network</span>
                  <span className="result-value">{selectedNetwork}</span>
                </div>
              </div>

              <button
                type="button"
                className="btn btn-primary btn-full"
                onClick={handleRegister}
                disabled={regLoading}
              >
                {regLoading ? 'Registering...' : 'Register Head'}
              </button>

              {regResult && (
                <motion.div
                  className="register-result success"
                  initial={{ opacity: 0, scale: 0.95 }}
                  animate={{ opacity: 1, scale: 1 }}
                  transition={{ duration: 0.3 }}
                >
                  <h3>Head registered successfully</h3>
                  <div className="result-details">
                    <div className="result-row">
                      <span className="result-label">Head ID</span>
                      <code className="result-value">{regResult.headId}</code>
                    </div>
                    <div className="result-row">
                      <span className="result-label">Status</span>
                      <span className="result-value">{regResult.status}</span>
                    </div>
                  </div>

                  <div style={{ marginTop: '1.5rem', padding: '1rem', background: 'var(--surface)', borderRadius: '8px', border: '1px solid var(--border)' }}>
                    <p style={{ fontSize: '0.9rem', color: 'var(--text-muted)', marginBottom: '0.75rem', fontWeight: 600 }}>What's next</p>
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem', fontSize: '0.9rem', color: 'var(--text-muted)' }}>
                      <p>
                        <strong style={{ color: 'var(--text)' }}>1. Publish the HTLC validator</strong> — if you operate this head,
                        publish the HTLC reference script UTxO inside it before payments can flow.
                        See <Link to="/setup" style={{ color: 'var(--accent)' }}>Setup guide → step 04</Link>.
                      </p>
                      <p>
                        <strong style={{ color: 'var(--text)' }}>2. Receive a payment</strong> — generate a secret, create an invoice
                        with your key hash and amount, and share the invoice ID with the sender.
                      </p>
                      <p>
                        <strong style={{ color: 'var(--text)' }}>3. Send a payment</strong> — enter an invoice ID and your key hash
                        to find a route and execute the HTLC cascade.
                      </p>
                    </div>
                  </div>

                  <div style={{ display: 'flex', gap: '0.5rem', marginTop: '1rem', flexWrap: 'wrap' }}>
                    <Link to="/invoice" className="btn btn-primary" style={{ flex: 1, textAlign: 'center' }}>
                      Create invoice
                    </Link>
                    <Link to="/routes" className="btn btn-secondary" style={{ flex: 1, textAlign: 'center' }}>
                      Find a route
                    </Link>
                    <button
                      type="button"
                      className="btn btn-secondary"
                      style={{ flex: 1 }}
                      onClick={resetWizard}
                    >
                      Register another
                    </button>
                  </div>
                </motion.div>
              )}

              {regError && (
                <motion.div
                  className="register-result error"
                  initial={{ opacity: 0, scale: 0.95 }}
                  animate={{ opacity: 1, scale: 1 }}
                  transition={{ duration: 0.3 }}
                >
                  <h3>Registration failed</h3>
                  <p>{regError}</p>
                </motion.div>
              )}
            </div>
          </motion.section>
        )}
      </AnimatePresence>

      {/* About the HTLC Contract (collapsible) */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.3, duration: 0.5 }}
      >
        <button
          type="button"
          className="about-htlc-toggle"
          onClick={() => setAboutOpen(!aboutOpen)}
        >
          <span className="about-htlc-toggle-icon">{aboutOpen ? '▼' : '▶'}</span>
          About the HTLC Contract
        </button>

        <AnimatePresence>
          {aboutOpen && (
            <motion.div
              initial={{ opacity: 0, height: 0 }}
              animate={{ opacity: 1, height: 'auto' }}
              exit={{ opacity: 0, height: 0 }}
              transition={{ duration: 0.3 }}
              style={{ overflow: 'hidden' }}
            >
              <div className="prerequisite-card glow-card">
                <h3>HTLC Contract</h3>
                <p className="register-desc" style={{ marginTop: '0.75rem' }}>
                  Registering a head as HTLC-capable is a policy declaration: bridge
                  operators that opt in commit to constructing on-the-fly lock and
                  claim transactions (with the validator inlined as a reference
                  script) at payment time, rather than pre-depositing the script
                  into the head. There is no on-chain HTLC state until a payment
                  flows through the bridge.
                </p>
                <div className="result-details">
                  <div className="result-row">
                    <span className="result-label">Script hash</span>
                    <code className="result-value result-value-mono">{HTLC_SCRIPT_HASH}</code>
                  </div>
                  <div className="result-row">
                    <span className="result-label">Source</span>
                    <a href={HTLC_REPO} className="result-value" target="_blank" rel="noopener noreferrer">{HTLC_REPO}</a>
                  </div>
                  <div className="result-row">
                    <span className="result-label">Plutus version</span>
                    <span className="result-value">V3 (Aiken)</span>
                  </div>
                </div>
              </div>
            </motion.div>
          )}
        </AnimatePresence>
      </motion.section>
    </div>
  )
}
