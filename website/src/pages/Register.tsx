import { useState } from 'react'
import { motion } from 'framer-motion'
import { registerHead } from '../api/client'

const HTLC_SCRIPT_HASH = '0cf48862039d41d87df4257a6861640d99168f7ab9fa294e6da9ab57'
const HTLC_REPO = 'https://github.com/v0d1ch/htlc'

// Pre-published reference UTxOs — fill in after publishing
const REFERENCE_UTXOS: Record<string, { txRef: string; address: string }> = {
  Mainnet: { txRef: 'TBD', address: 'TBD' },
  Preview: { txRef: 'TBD', address: 'TBD' },
  Preprod: { txRef: 'TBD', address: 'TBD' },
}

const REF_LOVELACE = 5_000_000

const NETWORKS = Object.keys(REFERENCE_UTXOS) as (keyof typeof REFERENCE_UTXOS)[]

export default function Register() {
  const [selectedNetwork, setSelectedNetwork] = useState<string>(NETWORKS[0])
  const [host, setHost] = useState('')
  const [port, setPort] = useState('')
  const [isBridge, setIsBridge] = useState(false)
  const [bridgeFee, setBridgeFee] = useState('')
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState<{ headId: string; status: string } | null>(null)
  const [error, setError] = useState<string | null>(null)

  const ref = REFERENCE_UTXOS[selectedNetwork]

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)
    setError(null)
    setResult(null)

    const portNum = parseInt(port, 10)
    if (isNaN(portNum) || portNum < 1 || portNum > 65535) {
      setError('Port must be a number between 1 and 65535')
      setLoading(false)
      return
    }

    let feeLovelace: number | undefined
    if (isBridge && bridgeFee) {
      feeLovelace = Math.round(parseFloat(bridgeFee) * 1_000_000)
      if (isNaN(feeLovelace) || feeLovelace < 0) {
        setError('Bridge fee must be a non-negative number')
        setLoading(false)
        return
      }
    }

    try {
      const res = await registerHead(host, portNum, isBridge || undefined, feeLovelace)
      setResult(res)
      const stored = JSON.parse(localStorage.getItem('registeredHeads') ?? '[]')
      stored.push({ headId: res.headId, host, port: portNum, isBridge, registeredAt: new Date().toISOString() })
      localStorage.setItem('registeredHeads', JSON.stringify(stored))
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Registration failed')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="register-page">
      {/* HTLC prerequisite section */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Before You Register</h1>
        <p className="register-desc">
          To participate in the payment relay, your Hydra head must contain the
          official HTLC contract as a reference UTxO. We have already published
          the contract on every network &mdash; you just need to commit it into
          your head.
        </p>

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
          <h3>Commit the HTLC contract into your head</h3>
          <p className="register-desc">
            Select your network, then run the command against your Hydra
            node&rsquo;s API. All head participants must agree on the initial
            UTxO set, so coordinate with other participants to include it.
          </p>

          <div className="network-selector register-network-selector">
            {NETWORKS.map(n => (
              <button
                key={n}
                className={`network-btn ${selectedNetwork === n ? 'network-active' : ''}`}
                onClick={() => setSelectedNetwork(n)}
              >
                {n}
              </button>
            ))}
          </div>

          <div className="result-details" style={{ marginTop: '1rem' }}>
            <div className="result-row">
              <span className="result-label">Reference UTxO</span>
              <code className="result-value result-value-mono">{ref.txRef}</code>
            </div>
          </div>

          <pre className="code-block" style={{ marginTop: '1rem' }}>{`curl -X POST http://localhost:4001/commit \\\n  -H 'Content-Type: application/json' \\\n  -d '{\n    "${ref.txRef}": {\n      "address": "${ref.address}",\n      "value": {"lovelace": ${REF_LOVELACE}},\n      "referenceScript": {\n        "hash": "${HTLC_SCRIPT_HASH}",\n        "type": "PlutusV3"\n      }\n    }\n  }'`}</pre>

          <p className="register-desc" style={{ marginTop: '1.5rem' }}>
            Once your head is open, register it using the form below. The
            registry automatically detects the HTLC script by its
            hash (<code>{HTLC_SCRIPT_HASH.slice(0, 16)}...</code>) and marks
            your head as HTLC-enabled. Verify on the{' '}
            <a href="/explorer">Explorer</a> page &mdash; look for the
            yellow HTLC badge.
          </p>
        </div>
      </motion.section>

      {/* Registration form */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.2, duration: 0.5 }}
      >
        <h2 className="section-title">Register Your Head</h2>
        <p className="register-desc">
          Once the HTLC contract is committed into your head, register it here.
        </p>

        <form className="register-form" onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="host">Host</label>
            <input
              id="host"
              type="text"
              placeholder="e.g. 192.168.1.100 or my-hydra-node.example.com"
              value={host}
              onChange={(e) => setHost(e.target.value)}
              required
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
              required
            />
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

          <button type="submit" className="btn btn-primary btn-full" disabled={loading}>
            {loading ? 'Connecting...' : 'Register Head'}
          </button>
        </form>

        {result && (
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
                <code className="result-value">{result.headId}</code>
              </div>
              <div className="result-row">
                <span className="result-label">Status</span>
                <span className="result-value">{result.status}</span>
              </div>
            </div>
          </motion.div>
        )}

        {error && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 0.3 }}
          >
            <h3>Registration failed</h3>
            <p>{error}</p>
          </motion.div>
        )}
      </motion.section>
    </div>
  )
}
