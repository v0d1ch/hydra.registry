import { motion } from 'framer-motion'

const HTLC_SCRIPT_HASH = '81b00e96189dc6dc1d492c469442d0fce05367e946a1b59de13a17df'
const HTLC_REPO = 'https://github.com/v0d1ch/htlc'

export default function Docs() {
  return (
    <div className="register-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">HTLC Lock & Claim Reference</h1>
        <p className="register-desc">
          A payment hops through one HTLC per Hydra head along the route. The
          registry coordinates discovery, watches for state changes, and (soon)
          builds the lock and claim transactions for you. This page is the
          ground-truth reference for what those transactions actually look like
          on the wire — useful if you operate a bridge, build your own client,
          or want to audit what the helper endpoints emit.
        </p>
      </motion.section>

      {/* The contract */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.1, duration: 0.5 }}
      >
        <h2 className="section-title">The HTLC contract</h2>
        <p className="register-desc">
          A single Plutus V3 validator (Aiken) parameterised by a datum that
          binds a payment hash, a timeout slot, and the two parties allowed to
          spend the locked output. There is no on-chain HTLC state until a
          payment flows: the validator script is reused per hop, inlined as a
          reference script in each lock output, and gone again once the hop is
          either claimed or refunded.
        </p>

        <div className="prerequisite-card glow-card">
          <h3>Validator</h3>
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
          <h3>Datum</h3>
          <pre className="code-block">{`Datum {
  hash:     ByteArray,           -- blake2b-256(preimage), 32 bytes
  timeout:  Int,                 -- absolute slot; claim before, refund after
  sender:   VerificationKeyHash, -- 28 bytes; allowed to refund after timeout
  receiver: VerificationKeyHash, -- 28 bytes; allowed to claim with preimage
}`}</pre>

          <h3 style={{ marginTop: '1.5rem' }}>Redeemer</h3>
          <pre className="code-block">{`Redeemer =
  | Claim(ByteArray)   -- preimage; blake2b-256(preimage) must equal datum.hash
  | Refund             -- no payload; only valid once timeout has passed`}</pre>

          <h3 style={{ marginTop: '1.5rem' }}>Spending rules</h3>
          <p className="register-desc">
            <strong>Claim(preimage)</strong> succeeds when{' '}
            <code>blake2b_256(preimage) == datum.hash</code>, the transaction's
            validity range upper bound is strictly before{' '}
            <code>datum.timeout</code>, and <code>datum.receiver</code> signs.
            <br />
            <strong>Refund</strong> succeeds when the validity range lower bound
            is strictly after <code>datum.timeout</code> and{' '}
            <code>datum.sender</code> signs.
          </p>
        </div>
      </motion.section>

      {/* Cascade */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.15, duration: 0.5 }}
      >
        <h2 className="section-title">The payment cascade</h2>
        <p className="register-desc">
          For an N-hop route, every hop locks the value{' '}
          <code>amount + fees_remaining_downstream</code> in its head, all keyed
          to the same payment hash. The receiver reveals the preimage on the
          last hop; bridge operators see the reveal in their head and use it to
          claim the upstream hop they were named in.
        </p>

        <div className="next-steps">
          <div className="next-step">
            <span className="next-num">1</span>
            <p>
              <strong>Receiver creates the invoice.</strong> They pick a 32-byte
              secret offline, compute <code>h = blake2b_256(secret)</code>, and
              post an invoice to <code>POST /api/v1/relay/invoices</code> with{' '}
              <code>h</code>, <code>amount</code>, and an expiry. The secret
              never leaves their machine.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">2</span>
            <p>
              <strong>Sender finds a route.</strong>{' '}
              <code>POST /api/v1/relay/routes</code> returns up to 3 ranked
              routes with hops, fees, and head IDs. The route is persisted with
              a shared timeout slot derived from the invoice expiry.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">3</span>
            <p>
              <strong>Sender locks hop 0.</strong> Inside the head at{' '}
              <code>route.hops[0].headId</code>, the sender produces an HTLC
              output with <code>datum.hash = h</code>,{' '}
              <code>datum.timeout = route.timeoutSlot</code>,{' '}
              <code>datum.sender = senderPkh</code>,{' '}
              <code>datum.receiver = bridge0Pkh</code>, and the validator
              attached as a reference script.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">4</span>
            <p>
              <strong>Each bridge locks the next hop.</strong> Bridge operator{' '}
              <code>i</code> sees its incoming HTLC inside head <code>i</code>{' '}
              (via the registry's HTLC watcher), then locks an outgoing HTLC in
              head <code>i+1</code> with the same <code>hash</code> and{' '}
              <code>timeout</code> but{' '}
              <code>sender = bridge[i]Pkh, receiver = bridge[i+1]Pkh</code>.
              Last hop's receiver is the invoice receiver.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">5</span>
            <p>
              <strong>Receiver claims the final hop</strong> with{' '}
              <code>Claim(preimage)</code>. They also{' '}
              <code>POST /api/v1/relay/preimage/&#123;paymentHash&#125;</code>{' '}
              so bridges that aren't watching their head's UTxO set directly
              still learn the secret.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">6</span>
            <p>
              <strong>Preimage cascades back.</strong> Each bridge claims its
              upstream HTLC in turn. If any hop is unclaimed by{' '}
              <code>timeout</code>, the locker submits a <code>Refund</code>{' '}
              transaction in that head and recovers their funds.
            </p>
          </div>
        </div>
      </motion.section>

      {/* Lock tx */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.2, duration: 0.5 }}
      >
        <h2 className="section-title">Lock transaction</h2>
        <p className="register-desc">
          Submitted via the locker's hydra-node WebSocket as a{' '}
          <code>NewTx</code> command. Lives entirely on L2 inside one head — no
          L1 settlement happens at lock time.
        </p>
        <pre className="code-block">{`tx_inputs:
  - one or more L2 UTxOs from the locker totalling >= amount + fees + min_ada

tx_outputs:
  - HTLC output:
      address      = script address of HTLC validator (network-prefixed)
      value        = amount + fees_remaining_downstream + min_ada
      datum_inline = Datum { hash, timeout, sender_pkh, receiver_pkh }
      ref_script   = HTLC validator (Plutus V3 bytes inlined)
  - change output back to the locker (if any)

validity_range:
  upper = timeout - safety_margin   (so the receiver's claim is valid)

required_signers:
  - the locker's payment key (to spend the input)`}</pre>
      </motion.section>

      {/* Claim tx */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.25, duration: 0.5 }}
      >
        <h2 className="section-title">Claim transaction</h2>
        <p className="register-desc">
          Spends an HTLC output by revealing the preimage. The receiver of the
          final hop runs this first; each upstream bridge runs the same shape
          against its own incoming hop once the preimage is visible.
        </p>
        <pre className="code-block">{`tx_inputs:
  - the HTLC UTxO being claimed (with its inline ref script reused)

tx_outputs:
  - one or more outputs to the claimer's address, total = HTLC value - tx_fee

redeemer:
  Claim(preimage)   -- 32 bytes; the original secret

validity_range:
  upper < datum.timeout              (claim must be before timeout)

required_signers:
  - datum.receiver  (claimer's payment key hash)`}</pre>
      </motion.section>

      {/* Refund tx */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.3, duration: 0.5 }}
      >
        <h2 className="section-title">Refund transaction</h2>
        <p className="register-desc">
          Reclaims a locked HTLC after its timeout has passed and no preimage
          surfaced. Bridges should refund stuck hops promptly to free up
          collateral; the cascade must move forward or unwind cleanly.
        </p>
        <pre className="code-block">{`tx_inputs:
  - the HTLC UTxO being refunded

tx_outputs:
  - one or more outputs to the locker's address, total = HTLC value - tx_fee

redeemer:
  Refund

validity_range:
  lower > datum.timeout              (refund must be after timeout)

required_signers:
  - datum.sender    (locker's payment key hash)`}</pre>
      </motion.section>

      {/* Helper endpoints */}
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.35, duration: 0.5 }}
      >
        <h2 className="section-title">Helper endpoints (coming soon)</h2>
        <p className="register-desc">
          The registry already stores everything needed to assemble the
          transactions above — head, bridge address, sender/receiver per hop,
          payment hash, timeout slot, fees. The next milestone exposes that as
          three POST endpoints that return unsigned tx CBOR for the caller to
          sign and submit via their hydra-node.
        </p>
        <div className="setup-steps">
          <div className="setup-step">
            <strong>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/lock-tx</strong>
            <p>
              Returns the unsigned Conway-era L2 transaction that locks hop{' '}
              <code>hopIndex</code> of the given route. Body provides the
              locker's vkey hash and a list of L2 UTxOs to spend; response is{' '}
              <code>{'{ cborHex, txId }'}</code>.
            </p>
          </div>
          <div className="setup-step">
            <strong>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/claim-tx</strong>
            <p>
              Returns the unsigned claim transaction for the given hop. Body
              provides the preimage and the claimer's change address.
            </p>
          </div>
          <div className="setup-step">
            <strong>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/refund-tx</strong>
            <p>
              Returns the unsigned refund transaction. Only valid after{' '}
              <code>timeout</code>; the response embeds the appropriate validity
              lower bound.
            </p>
          </div>
        </div>
      </motion.section>
    </div>
  )
}
