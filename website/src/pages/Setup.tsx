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
          Payments on Hydra use Hash Time-Lock Contracts (HTLCs) — trustless scripts that let a sender
          lock funds for a receiver, who claims them by revealing a secret. If the receiver doesn't
          claim before the timeout, the sender gets a refund. This guide covers what you need to
          participate as a sender, receiver, or bridge operator.
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
            <h2 style={{ margin: 0 }}>Run a Hydra node in an open head</h2>
          </div>
          <p className="register-desc">
            Each participant runs their own <code>hydra-node</code> connected to a shared Hydra Head.
            Transactions — including HTLC locks and claims — are submitted to the head and confirmed
            instantly by all participants' nodes via the multi-party snapshot protocol.
          </p>
          <p className="register-desc">
            Your node watches the head's UTxO set. When a lock transaction addressed to your key
            appears in a snapshot, your node has already confirmed it — no on-chain finality wait.
          </p>
          <p className="register-desc">
            Follow the{' '}
            <a href="https://hydra.family/head-protocol/docs/getting-started" target="_blank" rel="noopener noreferrer">
              official Hydra documentation
            </a>{' '}
            to set up your node with real keys and funds.
          </p>
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
            <h2 style={{ margin: 0 }}>Know your on-chain key hash</h2>
          </div>
          <p className="register-desc">
            Your <strong>key hash</strong> is a 28-byte hex string derived from your Cardano payment
            verification key. It is the identity that HTLC contracts are addressed to — both the
            lock transaction (which names you as receiver) and your claim transaction reference it.
          </p>
          <p className="register-desc">
            This is <em>not</em> a wallet address. The HTLC validator checks that the spending
            transaction is signed by the key that hashes to this value, so only you can claim funds
            locked to your key hash.
          </p>
          <pre className="code-block">{`cardano-cli address key-hash \\
  --payment-verification-key-file <your-actor>.vk`}</pre>
          <p className="register-desc" style={{ marginTop: '0.75rem' }}>
            Share this with anyone who needs to send you a payment or route through you. It is safe
            to publish — it's a hash, not a secret key.
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
            <h2 style={{ margin: 0 }}>Register your node with an indexer</h2>
          </div>
          <p className="register-desc">
            For payments to be routed to you, an indexer needs to know which heads you participate
            in and what funds you hold. The indexer connects to your hydra-node over its WebSocket
            API, reads the head state, and tracks each snapshot so the UTxO set stays current.
          </p>
          <p className="register-desc">
            You only need to register once per head. The indexer reconnects automatically if your
            node restarts.
          </p>
          <div className="setup-steps">
            <h3>What you need</h3>
            <ul style={{ color: 'var(--text-dim)', paddingLeft: '1.2rem', lineHeight: '1.8' }}>
              <li><strong>Host</strong> — the hostname or IP where your hydra-node is reachable (e.g. <code>localhost</code>)</li>
              <li><strong>Port</strong> — the hydra-node WebSocket API port (check your node startup flags)</li>
            </ul>
          </div>
          <Link to="/register" className="btn btn-primary" style={{ display: 'inline-block', marginTop: '1rem' }}>
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
            <h2 style={{ margin: 0 }}>Publish the HTLC validator inside the head <span style={{ fontSize: '0.7em', color: 'var(--text-muted)', fontWeight: 400 }}>(operator, one-time)</span></h2>
          </div>
          <p className="register-desc">
            Every HTLC lock and claim transaction references the same Plutus validator. Rather than
            embedding the full script in every transaction, it lives as a single
            <strong> reference script UTxO inside the head</strong> that all transactions point to.
            This is done once per head by whoever operates it.
          </p>
          <p className="register-desc">
            You need an address that <strong>has funds committed inside the head</strong> — the
            same address you used when committing UTxOs at head initialisation. The transaction is
            built and signed locally using the Cardano signing key for that address, then submitted
            to the head over its WebSocket API.
          </p>
          <div className="setup-steps">
            <h3>Step-by-step</h3>
            <div className="next-steps">
              <div className="next-step">
                <span className="next-num">1</span>
                <div>
                  <p>
                    <strong>Find your in-head address.</strong> This is the address you committed
                    funds from when the head was opened. You can see it on the{' '}
                    <Link to="/dashboard" style={{ color: 'var(--accent)' }}>Dashboard</Link> next
                    to your head, or ask the indexer directly:
                  </p>
                  <pre className="code-block">{`curl https://<indexer>/api/v1/heads/{headId}/addresses`}</pre>
                  <p>Pick the address you control — the one whose <code>.sk</code> key file you have.</p>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">2</span>
                <div>
                  <p><strong>Build the publish transaction.</strong> Pass your in-head address. The indexer selects a suitable UTxO from inside the head to pay fees:</p>
                  <pre className="code-block">{`curl -X POST https://<indexer>/api/v1/heads/{headId}/publish-ref-script-tx-cbor \\
  -H 'Content-Type: application/json' \\
  -d '{"walletAddress": "addr_test1..."}'`}</pre>
                  <p>The response contains an unsigned <code>cborHex</code> and a <code>txId</code>. Keep both.</p>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">3</span>
                <div>
                  <p><strong>Sign the transaction</strong> with the Cardano signing key for that address:</p>
                  <pre className="code-block">{`echo '{"type":"Tx ConwayEra","description":"","cborHex":"<cborHex>"}' > tx.raw

cardano-cli transaction sign \\
  --tx-file tx.raw \\
  --signing-key-file <your-address>.sk \\
  --out-file tx.signed`}</pre>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">4</span>
                <div>
                  <p><strong>Submit the signed transaction to the head.</strong> This creates the reference script UTxO inside the head's L2 state:</p>
                  <pre className="code-block">{`SIGNED_CBOR=$(cat tx.signed | jq -r .cborHex)

curl -X POST https://<indexer>/api/v1/heads/{headId}/submit \\
  -H 'Content-Type: application/json' \\
  -d "{\"signedCborHex\": \"$SIGNED_CBOR\"}"`}</pre>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num">5</span>
                <div>
                  <p><strong>Register the reference UTxO</strong> so the indexer knows which UTxO holds the script. Use the <code>txId</code> from step 2:</p>
                  <pre className="code-block">{`curl -X POST https://<indexer>/api/v1/heads/{headId}/ref-script \\
  -H 'Content-Type: application/json' \\
  -d '{"utxo": "<txId>#0"}'`}</pre>
                  <p>Once registered, all future lock transactions reference this UTxO automatically.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </motion.section>

      {/* HTLC flow explanation */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        variants={cardVariants}
        custom={4}
      >
        <div className="prerequisite-card glow-card">
          <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', marginBottom: '1rem' }}>
            <span className="step-num" style={{ fontSize: '1.5rem' }}>→</span>
            <h2 style={{ margin: 0 }}>What happens during a payment</h2>
          </div>
          <p className="register-desc">
            A multi-hop payment works as a cascade of HTLCs, all sharing the same secret hash.
            Timeouts decrease at each hop so earlier hops always expire after later ones, giving
            each bridge operator time to claim before their own refund window closes.
          </p>
          <div className="setup-steps">
            <div className="next-steps">
              <div className="next-step">
                <span className="next-num" style={{ background: 'rgba(0,212,170,0.15)', color: 'var(--success)' }}>🔒</span>
                <div>
                  <p><strong>Locking (sender side).</strong> The sender submits a transaction to the head that locks funds in the HTLC validator, naming the next hop's key hash as receiver and setting a timeout slot. Each intermediate bridge operator does the same in their own head, forwarding the lock toward the final receiver.</p>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num" style={{ background: 'rgba(0,212,170,0.15)', color: 'var(--success)' }}>🔑</span>
                <div>
                  <p><strong>Claiming (receiver side).</strong> The final receiver claims the HTLC by submitting a transaction that includes the secret preimage. The validator checks that <code>sha256(preimage) == secretHash</code> and that the receiver's signature is present. Once claimed, the preimage is visible in the head's snapshot — bridge operators use it to claim their own HTLCs back up the chain.</p>
                </div>
              </div>
              <div className="next-step">
                <span className="next-num" style={{ background: 'rgba(255,77,106,0.15)', color: 'var(--error)' }}>⏱</span>
                <div>
                  <p><strong>Timeout / refund.</strong> If the receiver does not claim before the timeout slot, the sender (or bridge operator) can reclaim by submitting after the deadline with only their own signature. No preimage is required for a refund path. Timeouts are set so refunds always become available before the preceding hop's window closes.</p>
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
        custom={5}
      >
        <div className="prerequisite-card glow-card" style={{ borderColor: 'var(--success)', textAlign: 'center' }}>
          <h2 style={{ color: 'var(--success)', marginBottom: '0.5rem' }}>You're ready</h2>
          <p className="register-desc">
            With a running node, a registered head, a published HTLC script, and your key hash in hand,
            you can lock and claim payments across Hydra heads.
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
