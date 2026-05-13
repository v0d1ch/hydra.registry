import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'

const cardVariants = {
  hidden: { opacity: 0, y: 20 },
  visible: (i: number) => ({
    opacity: 1,
    y: 0,
    transition: { delay: i * 0.1, duration: 0.5, ease: [0.25, 0.46, 0.45, 0.94] as [number, number, number, number] },
  }),
}

export default function Setup() {
  return (
    <div className="register-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Setup Guide</h1>
        <p className="register-desc">
          Before you can send or receive payments through hydra.registry, you need a few things in place.
          This guide walks you through every step from zero to your first payment.
        </p>
      </motion.section>

      {/* Step 1 */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={0}
      >
        <div className="prerequisite-card glow-card">
          <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', marginBottom: '1rem' }}>
            <span className="step-num" style={{ fontSize: '1.5rem' }}>01</span>
            <h2 style={{ margin: 0 }}>Run a Hydra node</h2>
          </div>
          <p className="register-desc">
            You need a <code>hydra-node</code> connected to an open Hydra Head on Cardano.
            Each participant in a head runs their own node. The registry communicates with your node
            over its WebSocket API.
          </p>
          <div className="setup-steps">
            <h3>Quick start (local testnet)</h3>
            <p className="register-desc">
              The repo includes a testnet harness. From the project root:
            </p>
            <pre className="code-block">{`# 1. Start a Cardano node (syncs from Mithril snapshot — minutes, not days)
cd testnet && ./run.sh

# 2. Launch two Hydra heads (Alice + Ida in Head A, Ida + Bob in Head B)
./hydra.sh

# 3. Open the heads and commit funds
./open-heads.sh`}</pre>
            <p className="register-desc" style={{ marginTop: '1rem' }}>
              For production or testnet use, follow the{' '}
              <a href="https://hydra.family/head-protocol/docs/getting-started" target="_blank" rel="noopener noreferrer">
                official Hydra documentation
              </a>{' '}
              to set up your node with real keys and funds.
            </p>
          </div>
        </div>
      </motion.section>

      {/* Step 2 */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={1}
      >
        <div className="prerequisite-card glow-card">
          <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', marginBottom: '1rem' }}>
            <span className="step-num" style={{ fontSize: '1.5rem' }}>02</span>
            <h2 style={{ margin: 0 }}>Get your Cardano key hash</h2>
          </div>
          <p className="register-desc">
            Your <strong>key hash</strong> is your routing identity — a 56-character hex string
            (28 bytes) derived from your hydra-node's Cardano signing key. The relay uses it to
            find which heads you're in and to address HTLC contracts to you.
          </p>
          <p className="register-desc">
            This is <em>not</em> a wallet address. It's the raw hash of your payment verification key.
          </p>
          <pre className="code-block">{`cardano-cli address key-hash \\
  --payment-verification-key-file <your-actor>.vk`}</pre>
          <p className="register-desc" style={{ marginTop: '0.75rem' }}>
            The output is a 56-character hex string. Keep it handy — you'll enter it on the
            Invoice, Routes, and Dashboard pages. It's saved in your browser automatically once entered.
          </p>
          <div className="register-result" style={{ marginTop: '1rem', background: 'rgba(255,255,255,0.03)' }}>
            <p style={{ margin: 0, fontSize: '0.85rem', color: 'var(--text-muted)' }}>
              Example: <code>8f3a2b1c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c</code>
            </p>
          </div>
        </div>
      </motion.section>

      {/* Step 3 */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={2}
      >
        <div className="prerequisite-card glow-card">
          <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', marginBottom: '1rem' }}>
            <span className="step-num" style={{ fontSize: '1.5rem' }}>03</span>
            <h2 style={{ margin: 0 }}>Register your head</h2>
          </div>
          <p className="register-desc">
            Tell the registry about your Hydra node so it can index your head's UTxOs and include
            it in payment routing. You only need to do this once per head — the registry reconnects
            automatically on restart.
          </p>
          <div className="setup-steps">
            <h3>What you need</h3>
            <ul style={{ color: 'var(--text-dim)', paddingLeft: '1.2rem', lineHeight: '1.8' }}>
              <li><strong>Host</strong> — the hostname or IP where your hydra-node is reachable (e.g. <code>localhost</code>)</li>
              <li><strong>Port</strong> — the hydra-node WebSocket API port (default varies; check your node startup flags)</li>
            </ul>
          </div>
          <p className="register-desc">
            The registry connects to your node, reads the current head state, and starts listening
            for snapshots. Your head's UTxOs will appear on the Balance and Explorer pages within seconds.
          </p>
          <Link to="/register" className="btn btn-primary" style={{ display: 'inline-block', marginTop: '0.5rem' }}>
            Register your head →
          </Link>
        </div>
      </motion.section>

      {/* Step 4 */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={3}
      >
        <div className="prerequisite-card glow-card">
          <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', marginBottom: '1rem' }}>
            <span className="step-num" style={{ fontSize: '1.5rem' }}>04</span>
            <h2 style={{ margin: 0 }}>Publish the HTLC script <span style={{ fontSize: '0.7em', color: 'var(--text-muted)', fontWeight: 400 }}>(operator, one-time per head)</span></h2>
          </div>
          <p className="register-desc">
            Before HTLC payments can flow through a head, the HTLC Plutus validator must exist
            as a <strong>reference script UTxO inside the head</strong>. This is a one-time setup
            done by whoever operates the head. If you're joining someone else's head, ask the
            operator — they may have already done this.
          </p>
          <div className="setup-steps">
            <h3>Step-by-step</h3>
            <div className="next-steps">
              <div className="next-step">
                <span className="next-num">1</span>
                <div>
                  <p><strong>Build the publish transaction.</strong> Call the registry with your in-head wallet address:</p>
                  <pre className="code-block">{`curl -X POST https://your-registry/api/v1/heads/{headId}/publish-ref-script-tx-cbor \\
  -H 'Content-Type: application/json' \\
  -d '{"walletAddress": "addr_test1..."}'`}</pre>
                  <p>You get back a <code>BuildResult</code> with an unsigned transaction <code>cborHex</code>.</p>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">2</span>
                <div>
                  <p><strong>Save and sign the transaction.</strong></p>
                  <pre className="code-block">{`# Save the cborHex as a cardano-cli envelope
echo '{"type":"Tx ConwayEra","description":"","cborHex":"<cborHex>"}' > tx.raw

# Sign with your actor key
cardano-cli transaction sign \\
  --tx-file tx.raw \\
  --signing-key-file <actor>.sk \\
  --out-file tx.signed`}</pre>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">3</span>
                <div>
                  <p><strong>Submit to the head.</strong> Get the signed CBOR from the envelope:</p>
                  <pre className="code-block">{`# Extract cborHex from the signed envelope
SIGNED_CBOR=$(cardano-cli transaction view --tx-file tx.signed | jq -r .cborHex)

curl -X POST https://your-registry/api/v1/heads/{headId}/submit \\
  -H 'Content-Type: application/json' \\
  -d "{\"signedCborHex\": \"$SIGNED_CBOR\"}"`}</pre>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">4</span>
                <div>
                  <p><strong>Register the UTxO.</strong> Use the <code>txId</code> from the build result:</p>
                  <pre className="code-block">{`curl -X POST https://your-registry/api/v1/heads/{headId}/ref-script \\
  -H 'Content-Type: application/json' \\
  -d '{"utxo": "<txId>#0"}'`}</pre>
                  <p>Done. Future lock transactions will use this UTxO and cost only ~2 ADA min-ada instead of ~7 ADA.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </motion.section>

      {/* Ready */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={4}
      >
        <div className="prerequisite-card glow-card" style={{ borderColor: 'var(--success)', textAlign: 'center' }}>
          <h2 style={{ color: 'var(--success)', marginBottom: '0.5rem' }}>You're ready</h2>
          <p className="register-desc">
            With a running node, a registered head, a published HTLC script, and your key hash in hand,
            you can send and receive payments across Hydra heads.
          </p>
          <div style={{ display: 'flex', gap: '1rem', justifyContent: 'center', flexWrap: 'wrap', marginTop: '1.5rem' }}>
            <Link to="/invoice" className="btn btn-primary">
              Receive a payment
            </Link>
            <Link to="/dashboard" className="btn btn-secondary">
              Open Dashboard
            </Link>
          </div>
        </div>
      </motion.section>
    </div>
  )
}
