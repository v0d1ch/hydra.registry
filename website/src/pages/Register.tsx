import { useState, useCallback, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { registerHead, checkHead } from '../api/client'

// Persist the in-progress wizard across navigation.
const WIZARD_STORAGE_KEY = 'registerWizard'

interface WizardState {
  step: number
  selectedNetwork: string
  host: string
  port: string
  isBridge: boolean
  bridgeFee: string
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

export default function Register() {
  const saved = loadWizardState()
  const [step, setStep] = useState(saved.step ?? 0)
  const [selectedNetwork, setSelectedNetwork] = useState<string>(saved.selectedNetwork ?? NETWORKS[1])
  const [host, setHost] = useState(saved.host ?? '')
  const [port, setPort] = useState(saved.port ?? '')
  const [isBridge, setIsBridge] = useState(saved.isBridge ?? false)
  const [bridgeFee, setBridgeFee] = useState(saved.bridgeFee ?? '')

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
      step, selectedNetwork, host, port, isBridge, bridgeFee,
    }
    try { localStorage.setItem(WIZARD_STORAGE_KEY, JSON.stringify(data)) } catch { /* ignore */ }
  }, [step, selectedNetwork, host, port, isBridge, bridgeFee])

  useEffect(() => {
    if (regResult) {
      try { localStorage.removeItem(WIZARD_STORAGE_KEY) } catch { /* ignore */ }
    }
  }, [regResult])

  const resetWizard = () => {
    try { localStorage.removeItem(WIZARD_STORAGE_KEY) } catch { /* ignore */ }
    setStep(0)
    setHost('')
    setPort('')
    setIsBridge(false)
    setBridgeFee('')
    setConnectError(null)
    setRegResult(null)
    setRegError(null)
  }

  const validateStep1 = useCallback((): string | null => {
    if (!host.trim()) return 'Host is required'
    const portNum = parseInt(port, 10)
    if (isNaN(portNum) || portNum < 1 || portNum > 65535) return 'Port must be a number between 1 and 65535'
    if (isBridge && bridgeFee) {
      const fee = parseFloat(bridgeFee)
      if (isNaN(fee) || fee < 0) return 'Bridge fee must be a non-negative number'
    }
    return null
  }, [host, port, isBridge, bridgeFee])

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
    let feeLovelace: number | undefined
    if (isBridge && bridgeFee) {
      feeLovelace = Math.round(parseFloat(bridgeFee) * 1_000_000)
    }

    try {
      const res = await registerHead(host, portNum, isBridge || undefined, feeLovelace)
      setRegResult(res)
      const stored = JSON.parse(localStorage.getItem('registeredHeads') ?? '[]')
      stored.push({ headId: res.headId, host, port: portNum, isBridge, registeredAt: new Date().toISOString() })
      localStorage.setItem('registeredHeads', JSON.stringify(stored))
    } catch (err) {
      setRegError(err instanceof Error ? err.message : 'Registration failed')
    } finally {
      setRegLoading(false)
    }
  }

  return (
    <div className="register-page">
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

              <div className="form-group bridge-toggle">
                <label className="toggle-label">
                  <input
                    type="checkbox"
                    checked={isBridge}
                    onChange={e => setIsBridge(e.target.checked)}
                  />
                  <span className="toggle-text">Register as bridge operator</span>
                </label>
                <span className="form-hint">
                  Bridge operators relay payments between heads and earn fees per hop.
                </span>
              </div>

              {isBridge && (
                <motion.div
                  className="form-group"
                  initial={{ opacity: 0, height: 0 }}
                  animate={{ opacity: 1, height: 'auto' }}
                  exit={{ opacity: 0, height: 0 }}
                  transition={{ duration: 0.2 }}
                >
                  <label htmlFor="bridgeFee">Bridge Fee (ADA per hop)</label>
                  <input
                    id="bridgeFee"
                    type="text"
                    placeholder="e.g. 0.5"
                    value={bridgeFee}
                    onChange={e => setBridgeFee(e.target.value)}
                  />
                  <span className="form-hint">
                    Fee charged per payment relayed through this head. Leave empty for 0.
                  </span>
                </motion.div>
              )}

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
                {isBridge && (
                  <div className="result-row">
                    <span className="result-label">Bridge Fee</span>
                    <span className="result-value">{bridgeFee || '0'} ADA/hop</span>
                  </div>
                )}
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
