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
          registry coordinates discovery, watches for state changes, and emits
          tx blueprints that callers (typically a bridge operator) assemble
          and submit to their <strong>own</strong> hydra-node — the registry
          never submits transactions. This page is the ground-truth
          reference for what those transactions look like on the wire.
          Each participant is identified by their <em>OnChainId</em> — the
          28-byte key hash of their hydra-node's{' '}
          <code>--cardano-signing-key</code> — which is what the receiver
          field in every HTLC datum refers to.
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
          A route is a sequence of heads connected by shared participants
          (the implicit bridges). For <code>E</code> graph edges we have{' '}
          <code>E+1</code> heads in the path and <code>E+1</code> HTLC locks
          — one per head. Every lock uses the same payment hash and timeout;
          the receiver claims the final HTLC and the preimage cascades back
          to the sender's head.
        </p>

        <div className="next-steps">
          <div className="next-step">
            <span className="next-num">1</span>
            <p>
              <strong>Receiver creates the invoice.</strong> They pick a
              32-byte secret offline, compute{' '}
              <code>h = blake2b_256(secret)</code>, and post to{' '}
              <code>POST /api/v1/relay/invoices</code> with <code>h</code>,
              their <strong>Cardano key hash</strong>, an amount and expiry.
              The secret never leaves their machine.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">2</span>
            <p>
              <strong>Sender finds a route.</strong>{' '}
              <code>POST /api/v1/relay/routes</code> takes the invoice id +
              the sender's Cardano key hash + network. Returns up to 3 ranked
              routes; each one is persisted as <code>E+1</code> hops with a
              shared timeout slot derived from the invoice expiry.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">3</span>
            <p>
              <strong>Sender locks hop 0.</strong> Inside the sender's head,
              produce an HTLC output with <code>datum.hash = h</code>,{' '}
              <code>datum.timeout = route.timeoutSlot</code>,{' '}
              <code>datum.sender = senderKeyHash</code>,{' '}
              <code>datum.receiver = bridge[0]KeyHash</code>, validator
              attached as a reference script.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">4</span>
            <p>
              <strong>Each bridge locks the next hop.</strong> Bridge{' '}
              <code>i</code> sees its incoming HTLC in head <code>i</code>{' '}
              (via the registry's HTLC watcher) and locks an outgoing HTLC in
              head <code>i+1</code> with the same <code>hash</code> and{' '}
              <code>timeout</code> but{' '}
              <code>sender = bridge[i]KeyHash, receiver = bridge[i+1]KeyHash</code>.
              The last hop's receiver is the invoice's receiver key hash.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">5</span>
            <p>
              <strong>Receiver claims the final hop</strong> with{' '}
              <code>Claim(preimage)</code>, signing with their
              <code>--cardano-signing-key</code> and outputting the value to
              whatever address they choose (typically their wallet bech32).
              They also{' '}
              <code>POST /api/v1/relay/preimage/&#123;paymentHash&#125;</code>{' '}
              so bridges not watching their head's UTxO set directly still
              learn the secret.
            </p>
          </div>
          <div className="next-step">
            <span className="next-num">6</span>
            <p>
              <strong>Preimage cascades back.</strong> Each bridge claims its
              upstream HTLC in turn. If any hop is unclaimed by{' '}
              <code>timeout</code>, the locker submits a <code>Refund</code>{' '}
              transaction in that head and recovers the funds.
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
          Submitted by the locker to their own hydra-node (HTTP{' '}
          <code>POST /transaction</code>, or <code>NewTx</code> over WS).
          Lives entirely on L2 inside one head — no L1 settlement happens at
          lock time.
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
        <h2 className="section-title">Helper endpoints</h2>
        <p className="register-desc">
          The registry already stores everything needed to assemble the
          transactions above — head, bridge address, sender/receiver per hop,
          payment hash, timeout slot, fees. The endpoints below expose that as{' '}
          <strong>blueprints</strong>: every protocol-specific field is
          pre-computed (datum CBOR, redeemer CBOR, script address, validity
          slot, required signer pkh, lock amount). Callers plug those into a
          tx body skeleton built by their own hydra-node helpers, then sign
          and submit to their own node.
        </p>
        <p className="register-desc">
          Prefer full unsigned tx CBOR? The <code>…-tx-cbor</code> variants of
          these endpoints build the complete Conway envelope server-side from
          agent-pushed protocol parameters — you only sign and submit.
          Blueprints remain for callers who assemble transactions with their
          own tooling.
        </p>

        <div className="setup-steps">
          <h3>GET /api/v1/htlc/validator</h3>
          <p className="register-desc">
            Returns the HTLC validator bytes (Plutus V3 CBOR), its 28-byte
            script hash, and the script type. Cache it once per network — the
            same validator is reused on every lock output as a reference
            script.
          </p>
          <pre className="code-block">{`{ "scriptHash":   "81b00e96...13a17df",
  "scriptCborHex": "5903d40101...",
  "scriptType":    "PlutusV3" }`}</pre>
        </div>

        <div className="setup-steps">
          <h3>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/lock-tx</h3>
          <p className="register-desc">
            Builds a lock-tx blueprint for hop <code>hopIndex</code> (one of
            the route's <code>E+1</code> hops). No request body. The locker
            is the hop's <em>sender</em>: the original payer for hop 0, the
            bridge of the previous hop otherwise.
          </p>
          <pre className="code-block">{`{ "headId":             "...",
  "scriptAddress":      "addr_test1wq...",
  "scriptHash":         "81b00e96...13a17df",
  "datum": {
    "paymentHash": "<32 bytes hex>",
    "timeoutSlot": 12345678,
    "senderPkh":   "<28 bytes hex>",
    "receiverPkh": "<28 bytes hex>"
  },
  "datumCborHex":              "d87984...",
  "validatorRefScriptCborHex": "",
  "lockAmountLovelace":  1500000,
  "validityUpperSlot":   12345618,
  "requiredSignerPkh":   "<28 bytes hex>" }`}</pre>
          <p className="register-desc">
            <code>lockAmountLovelace</code> = invoice amount + sum of fees of
            every downstream hop, so each bridge can shave its cut as the
            payment cascades forward. Inline <code>datumCborHex</code> in the
            HTLC output and attach the validator (from{' '}
            <code>/htlc/validator</code>) as the output's reference script.
            <code>validatorRefScriptCborHex</code> is left empty in the
            response to keep payloads small; fetch the validator once and
            cache it.
          </p>
        </div>

        <div className="setup-steps">
          <h3>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/claim-tx</h3>
          <p className="register-desc">
            Builds a claim-tx blueprint. Body:{' '}
            <code>{'{ "preimage": "<hex>" }'}</code>. Returns 409 if the hop
            hasn't been locked yet (no <code>htlc_tx_hash</code> recorded).
          </p>
          <pre className="code-block">{`{ "headId":            "...",
  "htlcInputTxHash":   "<txid hex>",
  "htlcInputIndex":    0,
  "redeemerCborHex":   "d87981...",
  "validityUpperSlot": 12345618,
  "requiredSignerPkh": "<receiver pkh hex>" }`}</pre>
          <p className="register-desc">
            The on-chain validator already knows the datum (it's inline on
            the HTLC UTxO) — you only need to feed it the redeemer and a
            tx that satisfies <code>validity.upper &lt; timeout</code> and is
            signed by <code>requiredSignerPkh</code>.
          </p>
        </div>

        <div className="setup-steps">
          <h3>POST /api/v1/relay/payments/{'{routeId}'}/hops/{'{hopIndex}'}/refund-tx</h3>
          <p className="register-desc">
            Builds a refund-tx blueprint. No request body. Same 409 behavior
            if the hop wasn't locked.
          </p>
          <pre className="code-block">{`{ "headId":            "...",
  "htlcInputTxHash":   "<txid hex>",
  "htlcInputIndex":    0,
  "redeemerCborHex":   "d87a80",
  "validityLowerSlot": 12345738,
  "requiredSignerPkh": "<sender pkh hex>" }`}</pre>
          <p className="register-desc">
            The refund is only acceptable to the validator once{' '}
            <code>validity.lower &gt; timeout</code>; the registry adds a
            small slot safety margin so submission has slack.
          </p>
        </div>
      </motion.section>
    </div>
  )
}
