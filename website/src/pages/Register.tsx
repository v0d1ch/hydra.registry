import { useState } from 'react'
import { motion } from 'framer-motion'
import { registerHead } from '../api/client'

export default function Register() {
  const [host, setHost] = useState('')
  const [port, setPort] = useState('')
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState<{ headId: string; status: string } | null>(null)
  const [error, setError] = useState<string | null>(null)

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

    try {
      const res = await registerHead(host, portNum)
      setResult(res)
      // Store in localStorage for future reference
      const stored = JSON.parse(localStorage.getItem('registeredHeads') ?? '[]')
      stored.push({ headId: res.headId, host, port: portNum, registeredAt: new Date().toISOString() })
      localStorage.setItem('registeredHeads', JSON.stringify(stored))
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Registration failed')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="register-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Register a Hydra Head</h1>
        <p className="register-desc">
          Connect your running Hydra node to the registry. We&apos;ll connect via WebSocket,
          validate the head, and start indexing UTxO snapshots.
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

      <motion.section
        className="section"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.3, duration: 0.5 }}
      >
        <h2 className="section-title">What happens next?</h2>
        <div className="next-steps">
          <div className="next-step">
            <span className="next-num">1</span>
            <p>We connect to your Hydra node via WebSocket and verify it responds with a valid Greetings message.</p>
          </div>
          <div className="next-step">
            <span className="next-num">2</span>
            <p>The indexer starts listening for SnapshotConfirmed events and stores UTxO state in our database.</p>
          </div>
          <div className="next-step">
            <span className="next-num">3</span>
            <p>Wallets can now query your head&apos;s UTxOs through our Blockfrost and Yoroi-compatible endpoints.</p>
          </div>
        </div>
      </motion.section>
    </div>
  )
}
