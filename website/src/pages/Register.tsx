import { useState, useEffect, useCallback } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { registerHead, requestDeposit, checkHead } from '../api/client'

const HTLC_SCRIPT_HASH = '81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df'
const HTLC_SCRIPT_CBOR = '5903d401010029800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a4888888888c96600264653001300a00198051805800cdc3a4005300a0024888966002600460146ea800e2646644b300100789919912cc004c00c006264b300100180744c96600200300f807c03e01f13259800980c001c01602080a8dd7000a0303015001404c60226ea802a2b300130080018acc004c044dd5005400a01a809201a807100e0acc004c004c038dd50014660026024601e6ea800a46026602860286028602860286028602860280032232330010010032259800800c528456600266e3cdd7180b000801c528c4cc008008c05c005010202891809980a000c8c04cc050c050c050c050c050c050c0500052222259800980318099baa00d8992cc004cc0052410f505245494d414745204641494c4544005980099b8f37286eb8c060c054dd50071bae30183015375400d14a3153301349012b626c616b6532625f32353628707265696d61676529203d3d20646174756d2e68617368203f2046616c73650014a080922b3001330014910c56414c4944204245464f5245005980099191919912cc004c044c064dd50014566002602260326ea8c074c07800e266e20dd6980e980d1baa002001899b89375a603a60346ea80080050174528202e301b001375a600c60306ea8024cc064c068004cc0666002601c602c6ea8c068c06c00698103d87a8000a60103d8798000405097ae0301637546008602c6ea8004c008c054dd5005c528c54cc04d24013676616c69645f6265666f72652874782e76616c69646974795f72616e67652c20646174756d2e74696d656f757429203f2046616c73650014a080922660086eb0c014c054dd50059bae30183019301930193015375400d14a080922941012112cc00400629462a660280042c809a2b30013232323322598009808180c1baa0028acc004c040c060dd5180e180e801c4cdc40009bad301c301937540051337120026eb4c070c064dd5001202c8a50405860340026eb4c014c05cdd50041980c180c8009980c4c004c034c054dd5180c980d000d30103d87a8000a60103d8798000404c97ae0301537546030602a6ea8004c004c050dd500544cc00cdd61802180a1baa00a375c602e6030603060286ea801629410112022454cc0352411e65787065637420536f6d6528646174756d29203d20646174756d5f6f707400164030601c6ea8020dc3a400100a805402a0148098c03c004c03cc040004c02cdd5001c590080c028004c014dd5005c5268a99801a491856616c696461746f722072657475726e65642066616c7365001365640082a6600492011272656465656d65723a2052656465656d6572001601'
const HTLC_REPO = 'https://github.com/v0d1ch/htlc'

// Pre-published reference UTxOs
const REFERENCE_UTXOS: Record<string, { txRef: string; address: string }> = {
  Mainnet: { txRef: 'TBD', address: 'TBD' },
  Preview: { txRef: 'caaa5194116c2dc1c9f738cef6218c6bcf4a59937c220660ebb7d386a91a234e#0', address: 'addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8' },
  Preprod: { txRef: 'dd5b89fec6679046ddb17377e070ff306a120eb5f496ff8b9dff05a4e904ba63#0', address: 'addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8' },
}

const REF_LOVELACE = 6_000_000

const NETWORKS = Object.keys(REFERENCE_UTXOS) as (keyof typeof REFERENCE_UTXOS)[]

const KNOWN_WALLETS = ['eternl', 'nami', 'flint', 'lace', 'yoroi', 'typhon', 'gerowallet', 'nufi']

const STEPS = ['Connection', 'HTLC Deposit', 'Register']

// Extend window type for CIP-30
declare global {
  interface Window {
    cardano?: Record<string, {
      name: string
      icon: string
      enable: () => Promise<{
        signTx: (tx: string, partialSign: boolean) => Promise<string>
        submitTx: (tx: string) => Promise<string>
      }>
    }>
  }
}

export default function Register() {
  const [step, setStep] = useState(0)
  const [selectedNetwork, setSelectedNetwork] = useState<string>(NETWORKS[1]) // Preview default
  const [host, setHost] = useState('')
  const [port, setPort] = useState('')
  const [isBridge, setIsBridge] = useState(false)
  const [bridgeFee, setBridgeFee] = useState('')

  // Deposit state
  const [depositLoading, setDepositLoading] = useState(false)
  const [depositCbor, setDepositCbor] = useState<string | null>(null)
  const [depositError, setDepositError] = useState<string | null>(null)
  const [depositMessage, setDepositMessage] = useState<string | null>(null)

  // CIP-30 wallet state
  const [availableWallets, setAvailableWallets] = useState<string[]>([])
  const [selectedWallet, setSelectedWallet] = useState<string>('')
  const [walletLoading, setWalletLoading] = useState(false)
  const [txHash, setTxHash] = useState<string | null>(null)
  const [walletError, setWalletError] = useState<string | null>(null)

  // Registration state
  const [regLoading, setRegLoading] = useState(false)
  const [regResult, setRegResult] = useState<{ headId: string; status: string } | null>(null)
  const [regError, setRegError] = useState<string | null>(null)

  // About section
  const [aboutOpen, setAboutOpen] = useState(false)

  // Detect CIP-30 wallets
  useEffect(() => {
    const found = KNOWN_WALLETS.filter(name => window.cardano && window.cardano[name])
    setAvailableWallets(found)
    if (found.length > 0) setSelectedWallet(found[0])
  }, [])

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
      setDepositError(err)
      return
    }
    setDepositError(null)
    setCheckLoading(true)

    try {
      const portNum = parseInt(port, 10)
      const res = await checkHead(host, portNum)
      if (res.alreadyRegistered) {
        setDepositError(`Head ${res.headId} is already registered.`)
        return
      }
      setStep(1)
    } catch (e) {
      setDepositError(e instanceof Error ? e.message : 'Failed to connect to Hydra node')
    } finally {
      setCheckLoading(false)
    }
  }

  const handlePrepareDeposit = async () => {
    setDepositLoading(true)
    setDepositError(null)
    setDepositCbor(null)
    setDepositMessage(null)

    try {
      const portNum = parseInt(port, 10)
      const res = await requestDeposit({ host, port: portNum, network: selectedNetwork })
      setDepositCbor(res.depositTxCbor)
      setDepositMessage(res.message)
    } catch (err) {
      setDepositError(err instanceof Error ? err.message : 'Failed to prepare deposit')
    } finally {
      setDepositLoading(false)
    }
  }

  const handleSignAndSubmit = async () => {
    if (!depositCbor || !selectedWallet) return
    setWalletLoading(true)
    setWalletError(null)
    setTxHash(null)

    try {
      const walletApi = window.cardano?.[selectedWallet]
      if (!walletApi) throw new Error(`Wallet "${selectedWallet}" not found`)

      const api = await walletApi.enable()
      const signedTx = await api.signTx(depositCbor, true)
      const hash = await api.submitTx(signedTx)
      setTxHash(hash)
    } catch (err) {
      setWalletError(err instanceof Error ? err.message : 'Wallet signing failed')
    } finally {
      setWalletLoading(false)
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

  const handleCopyCbor = () => {
    if (depositCbor) {
      navigator.clipboard.writeText(depositCbor)
    }
  }

  const ref = REFERENCE_UTXOS[selectedNetwork]

  return (
    <div className="register-page">
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
            <span className="wizard-step-num">{i < step ? '\u2713' : i + 1}</span>
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

              {depositError && step === 0 && (
                <div className="register-result error" style={{ marginBottom: '1rem' }}>
                  <p>{depositError}</p>
                </div>
              )}

              <button type="button" className="btn btn-primary btn-full" onClick={handleNextStep1} disabled={checkLoading}>
                {checkLoading ? 'Checking...' : 'Continue to Deposit'}
              </button>
            </div>
          </motion.section>
        )}

        {/* ─── Step 2: HTLC Deposit ─── */}
        {step === 1 && (
          <motion.section
            key="step-1"
            className="section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: 20 }}
            transition={{ duration: 0.3 }}
          >
            <h1 className="section-title">HTLC Deposit</h1>
            <p className="register-desc">
              Prepare and submit the HTLC deposit transaction for your head
              on <strong>{selectedNetwork}</strong>.
            </p>

            <div className="register-form">
              {!depositCbor && (
                <>
                  <p className="deposit-info-text">
                    This will build a deposit transaction that includes the HTLC contract
                    as a reference script in your Hydra head at{' '}
                    <code>{host}:{port}</code>.
                  </p>
                  <button
                    type="button"
                    className="btn btn-primary btn-full"
                    onClick={handlePrepareDeposit}
                    disabled={depositLoading}
                  >
                    {depositLoading ? 'Preparing Deposit...' : 'Prepare HTLC Deposit'}
                  </button>
                </>
              )}

              {depositError && step === 1 && (
                <div className="register-result error" style={{ marginTop: '1rem' }}>
                  <h3>Deposit failed</h3>
                  <p>{depositError}</p>
                </div>
              )}

              {depositCbor && (
                <motion.div
                  initial={{ opacity: 0, y: 10 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ duration: 0.3 }}
                >
                  {depositMessage && (
                    <div className="register-result success" style={{ marginBottom: '1.5rem', marginTop: 0 }}>
                      <p>{depositMessage}</p>
                    </div>
                  )}

                  {/* Option A: CIP-30 Wallet */}
                  <div className="deposit-option">
                    <h3 className="deposit-option-title">Option A: Sign with Browser Wallet</h3>
                    {availableWallets.length === 0 ? (
                      <p className="deposit-no-wallet">
                        No CIP-30 wallets detected. Install a Cardano wallet extension
                        (Eternl, Nami, Lace, etc.) or use the manual option below.
                      </p>
                    ) : (
                      <div className="wallet-sign-row">
                        <div className="form-group" style={{ marginBottom: 0, flex: 1 }}>
                          <label htmlFor="wallet-select">Wallet</label>
                          <select
                            id="wallet-select"
                            className="wallet-select"
                            value={selectedWallet}
                            onChange={e => setSelectedWallet(e.target.value)}
                          >
                            {availableWallets.map(w => (
                              <option key={w} value={w}>{w.charAt(0).toUpperCase() + w.slice(1)}</option>
                            ))}
                          </select>
                        </div>
                        <button
                          type="button"
                          className="btn btn-primary"
                          onClick={handleSignAndSubmit}
                          disabled={walletLoading}
                          style={{ alignSelf: 'flex-end' }}
                        >
                          {walletLoading ? 'Signing...' : 'Sign & Submit'}
                        </button>
                      </div>
                    )}

                    {walletError && (
                      <div className="register-result error" style={{ marginTop: '1rem' }}>
                        <h3>Wallet error</h3>
                        <p>{walletError}</p>
                      </div>
                    )}

                    {txHash && (
                      <motion.div
                        className="register-result success"
                        style={{ marginTop: '1rem' }}
                        initial={{ opacity: 0, scale: 0.95 }}
                        animate={{ opacity: 1, scale: 1 }}
                      >
                        <h3>Transaction submitted</h3>
                        <div className="result-details">
                          <div className="result-row">
                            <span className="result-label">Tx Hash</span>
                            <code className="result-value result-value-mono">{txHash}</code>
                          </div>
                        </div>
                      </motion.div>
                    )}
                  </div>

                  {/* Option B: Manual */}
                  <details className="deposit-option deposit-manual">
                    <summary className="deposit-option-title deposit-manual-summary">
                      Option B: Manual (cardano-cli)
                    </summary>
                    <div className="deposit-manual-content">
                      <p className="deposit-info-text" style={{ marginBottom: '0.75rem' }}>
                        Copy the CBOR hex below and sign it with <code>cardano-cli</code>:
                      </p>
                      <div className="cbor-block-wrapper">
                        <pre className="code-block cbor-block">{depositCbor}</pre>
                        <button type="button" className="btn btn-secondary cbor-copy-btn" onClick={handleCopyCbor}>
                          Copy
                        </button>
                      </div>
                      <pre className="code-block" style={{ marginTop: '1rem' }}>{`# Sign the transaction
cardano-cli transaction sign \\
  --tx-body-file tx.raw \\
  --signing-key-file payment.skey \\
  --out-file tx.signed

# Submit
cardano-cli transaction submit \\
  --tx-file tx.signed`}</pre>
                    </div>
                  </details>

                  {/* Continue button */}
                  <button
                    type="button"
                    className="btn btn-primary btn-full"
                    style={{ marginTop: '1.5rem' }}
                    onClick={() => setStep(2)}
                  >
                    Continue to Registration
                  </button>
                </motion.div>
              )}
            </div>
          </motion.section>
        )}

        {/* ─── Step 3: Register ─── */}
        {step === 2 && (
          <motion.section
            key="step-2"
            className="section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: 20 }}
            transition={{ duration: 0.3 }}
          >
            <h1 className="section-title">Register Head</h1>
            <p className="register-desc">
              After the deposit transaction is confirmed, register your head
              at <code>{host}:{port}</code> on <strong>{selectedNetwork}</strong>.
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
          <span className="about-htlc-toggle-icon">{aboutOpen ? '\u25BC' : '\u25B6'}</span>
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

              <div className="setup-steps">
                <h3>Reference UTxOs by Network</h3>
                <div className="result-details" style={{ marginTop: '1rem' }}>
                  {NETWORKS.map(n => (
                    <div key={n} className="result-row">
                      <span className="result-label">{n}</span>
                      <code className="result-value result-value-mono" style={{ fontSize: '0.75rem' }}>
                        {REFERENCE_UTXOS[n].txRef}
                      </code>
                    </div>
                  ))}
                </div>

                <details style={{ marginTop: '1rem' }}>
                  <summary className="btn btn-secondary" style={{ cursor: 'pointer', listStyle: 'none' }}>Show curl command</summary>
                  <pre className="code-block" style={{ marginTop: '0.5rem' }}>{`curl -X POST http://<HYDRA_API_HOST>:<HYDRA_API_PORT>/commit \\
  -H 'Content-Type: application/json' \\
  -d '{
    "${ref.txRef}": {
      "address": "${ref.address}",
      "value": {"lovelace": ${REF_LOVELACE}},
      "referenceScript": {
        "script": {
          "cborHex": "${HTLC_SCRIPT_CBOR}",
          "description": "",
          "type": "PlutusScriptV3"
        },
        "type": "PlutusV3"
      }
    }
  }'`}</pre>
                </details>
              </div>
            </motion.div>
          )}
        </AnimatePresence>
      </motion.section>
    </div>
  )
}
