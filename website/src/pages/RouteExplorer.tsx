import { useEffect, useMemo, useState } from 'react'
import { motion } from 'framer-motion'
import { Link, useSearchParams } from 'react-router-dom'
import {
  findRoutes,
  executeRoute,
  getRelayGraph,
  getHeads,
  getExplorerHead,
  getHeadParticipants,
  getRegisteredHead,
  type RouteResponse,
  type PaymentStatusResponse,
  type SubgraphResponse,
  type SubgraphNode,
} from '../api/client'
import { useNetwork } from '../context/NetworkContext'
import RelayGraph from '../components/RelayGraph'

export default function RouteExplorer() {
  const { network } = useNetwork()
  const [searchParams, setSearchParams] = useSearchParams()
  const headIdFilter = searchParams.get('headId')?.trim() ?? ''
  const [headIdInput, setHeadIdInput] = useState(headIdFilter)
  const [registeredHeads, setRegisteredHeads] = useState<{ headId: string }[]>([])

  useEffect(() => {
    getHeads().then(heads => setRegisteredHeads(heads.map(h => ({ headId: h.headId })))).catch(() => {})
  }, [])
  const [invoiceId, setInvoiceId] = useState('')
  const [senderOnChainId, setSenderOnChainId] = useState('')
  const [loading, setLoading] = useState(false)
  const [routes, setRoutes] = useState<RouteResponse[] | null>(null)
  const [executing, setExecuting] = useState<string | null>(null)
  const [paymentResult, setPaymentResult] = useState<PaymentStatusResponse | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [graphData, setGraphData] = useState<SubgraphResponse | null>(null)
  const [graphLoading, setGraphLoading] = useState(false)
  const [graphError, setGraphError] = useState<string | null>(null)
  // Stand-alone synthesized node for an isolated head (no shared participants
  // → no edges → not present in /relay/graph). We fetch its metadata directly
  // so the user can still see their head on the page.
  const [standaloneNode, setStandaloneNode] = useState<SubgraphNode | null>(null)
  const [standaloneLoading, setStandaloneLoading] = useState(false)

  // Keep the URL in sync if the user navigates directly with a different ?headId
  useEffect(() => {
    setHeadIdInput(headIdFilter)
  }, [headIdFilter])

  const applyHeadFilter = (id: string) => {
    const trimmed = id.trim()
    const next = new URLSearchParams(searchParams)
    if (trimmed) next.set('headId', trimmed)
    else next.delete('headId')
    setSearchParams(next, { replace: false })
  }

  // Subset the graph to the focal head + its 1-hop neighbours. If the focal
  // head is isolated (no edges, so absent from /relay/graph), splice in the
  // standalone node we fetched directly so it still renders as a single dot.
  const filteredGraph = useMemo<SubgraphResponse | null>(() => {
    if (!graphData) return null
    if (!headIdFilter) return graphData
    const focalEdges = graphData.edges.filter(
      e => e.fromHead === headIdFilter || e.toHead === headIdFilter,
    )
    const keptIds = new Set<string>([headIdFilter])
    focalEdges.forEach(e => {
      keptIds.add(e.fromHead)
      keptIds.add(e.toHead)
    })
    const matchingNodes = graphData.nodes.filter(n => keptIds.has(n.headId))
    const focalAlreadyIn = matchingNodes.some(n => n.headId === headIdFilter)
    const nodes = focalAlreadyIn || !standaloneNode
      ? matchingNodes
      : [standaloneNode, ...matchingNodes]
    return { nodes, edges: focalEdges }
  }, [graphData, headIdFilter, standaloneNode])

  // Fetch standalone metadata when the focal head doesn't appear in the
  // /relay/graph response (no edges). We try the explorer first (most
  // detailed), then fall back to the registered-heads view (covers heads
  // that are registered locally but the external explorer sidecar hasn't
  // observed yet).
  useEffect(() => {
    if (!headIdFilter || !graphData) {
      setStandaloneNode(null)
      return
    }
    if (graphData.nodes.some(n => n.headId === headIdFilter)) {
      setStandaloneNode(null)
      return
    }
    let cancelled = false
    setStandaloneLoading(true)
    ;(async () => {
      try {
        const explorer = await getExplorerHead(headIdFilter).catch(() => null)
        const participants = await getHeadParticipants(headIdFilter).catch(() => [] as never[])
        if (cancelled) return
        if (explorer) {
          setStandaloneNode({
            headId: explorer.headId,
            network: explorer.network,
            hasHtlc: explorer.htlcEnabled,
            isUserHead: false,
            participants: participants.map(p => p.address),
            // Prefer the head-level L1-scan total; per-participant commit
            // amounts are rarely known (explorer members parsing).
            committedLovelace:
              explorer.totalValueLovelace ||
              participants.reduce((acc, p) => acc + (p.committedLovelace ?? 0), 0),
          })
          return
        }
        // Explorer hasn't seen this head yet — fall back to the registered
        // view so a freshly-registered head shows up immediately.
        const reg = await getRegisteredHead(headIdFilter).catch(() => null)
        if (cancelled) return
        if (!reg) {
          setStandaloneNode(null)
          return
        }
        setStandaloneNode({
          headId: reg.headId,
          network: reg.onChain?.network ?? (network !== 'All' ? network : 'Unknown'),
          hasHtlc: reg.onChain?.htlcEnabled ?? false,
          isUserHead: true,
          participants: participants.map(p => p.address),
          committedLovelace: participants.reduce(
            (acc, p) => acc + (p.committedLovelace ?? 0),
            0,
          ),
        })
      } finally {
        if (!cancelled) setStandaloneLoading(false)
      }
    })()
    return () => { cancelled = true }
  }, [headIdFilter, graphData, network])

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)
    setError(null)
    setRoutes(null)
    setPaymentResult(null)

    if (network === 'All') {
      setError('Please select a specific network (Mainnet, Preview, or Preprod) in the navbar.')
      setLoading(false)
      return
    }

    try {
      const res = await findRoutes({
        invoiceId,
        senderOnChainId,
        network,
      })
      setRoutes(res)
      if (res.length === 0) {
        setError('No routes found between these addresses on the selected network.')
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to find routes')
    } finally {
      setLoading(false)
    }
  }

  // Load relay graph when network changes
  useEffect(() => {
    if (network === 'All') {
      setGraphData(null)
      setGraphError(null)
      return
    }
    setGraphLoading(true)
    setGraphError(null)
    getRelayGraph(network)
      .then(res => {
        setGraphData(res)
        if (res.nodes.length === 0) {
          setGraphError('No relay heads found on this network.')
        }
      })
      .catch(err => setGraphError(err instanceof Error ? err.message : 'Failed to load graph'))
      .finally(() => setGraphLoading(false))
  }, [network])

  const handleExecute = async (routeId: string) => {
    setExecuting(routeId)
    setError(null)
    try {
      const res = await executeRoute(routeId)
      setPaymentResult(res)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to execute route')
    } finally {
      setExecuting(null)
    }
  }

  return (
    <div className="route-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">Head Network Graph</h1>
        <p className="register-desc">
          Interactive map of all open Hydra heads{network !== 'All' ? ` on ${network}` : ''}, not just those running HTLC.
          Nodes are linked when heads share participants. Drag to rearrange, hover to see connections, click for details.
        </p>

        {network === 'All' && (
          <div className="relay-graph-empty">
            Select a specific network (Mainnet, Preview, or Preprod) in the navbar to view the graph.
          </div>
        )}

        {graphLoading && (
          <div className="stats-loading">
            <div className="loading-spinner" />
            <p>Loading relay graph...</p>
          </div>
        )}

        {graphError && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
          >
            <p>{graphError}</p>
          </motion.div>
        )}

        {filteredGraph && filteredGraph.nodes.length === 0 && headIdFilter && !graphLoading && !standaloneLoading && (
          <div className="relay-graph-empty">
            Head <code>{headIdFilter}</code> not found on {network} (no record in the explorer or registry).
          </div>
        )}

        {filteredGraph && filteredGraph.nodes.length > 0 && (
          <motion.div
            initial={{ opacity: 0, y: 15 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.4 }}
          >
            <RelayGraph nodes={filteredGraph.nodes} edges={filteredGraph.edges} />
          </motion.div>
        )}

        {headIdFilter && (
          <div className="register-result" style={{ marginTop: '1rem' }}>
            <p>
              Showing routes through head <code>{headIdFilter}</code>
              {filteredGraph && filteredGraph.nodes.length > 0 && (
                <>
                  {' '}
                  · {filteredGraph.edges.length === 0
                    ? 'isolated (no neighbours yet — register a second head sharing a participant to see edges)'
                    : `${filteredGraph.nodes.length - 1} neighbour${filteredGraph.nodes.length === 2 ? '' : 's'} · ${filteredGraph.edges.length} edge${filteredGraph.edges.length === 1 ? '' : 's'}`}
                </>
              )}
            </p>
          </div>
        )}

        <form
          className="register-form"
          onSubmit={e => { e.preventDefault(); applyHeadFilter(headIdInput) }}
          style={{ marginTop: '1.5rem' }}
        >
          <div className="form-group">
            <label htmlFor="headIdFilter">Filter by Head ID</label>
            <input
              id="headIdFilter"
              type="text"
              placeholder="paste a head id to focus on it and its 1-hop neighbours"
              value={headIdInput}
              onChange={e => setHeadIdInput(e.target.value)}
              list="registered-heads"
            />
            <datalist id="registered-heads">
              {registeredHeads.map(h => (
                <option key={h.headId} value={h.headId}>
                  {h.headId}
                </option>
              ))}
            </datalist>
            <span className="form-hint">
              The URL stays in sync (<code>?headId=…</code>) so you can deep-link or share a focused view.
            </span>
          </div>
          <div style={{ display: 'flex', gap: '0.5rem' }}>
            <button type="submit" className="btn btn-primary">Filter</button>
            {headIdFilter && (
              <button
                type="button"
                className="btn btn-secondary"
                onClick={() => { setHeadIdInput(''); applyHeadFilter('') }}
              >
                Clear
              </button>
            )}
          </div>
        </form>
      </motion.section>

      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5, delay: 0.2 }}
      >
        <h2 className="section-title">Find Payment Route</h2>
        <p className="register-desc">
          Find the cheapest route to pay an invoice through bridge operators.
          Routes are computed using Dijkstra pathfinding weighted by bridge fees.
        </p>

        <form className="register-form" onSubmit={handleSearch}>
          <div className="form-group">
            <label htmlFor="invoiceId">Invoice ID</label>
            <input
              id="invoiceId"
              type="text"
              placeholder="Invoice ID from the receiver"
              value={invoiceId}
              onChange={e => setInvoiceId(e.target.value)}
              required
            />
          </div>
          <div className="form-group">
            <label htmlFor="senderKeyHash">Your Cardano Key Hash (28 bytes hex)</label>
            <input
              id="senderKeyHash"
              type="text"
              placeholder="56-char hex"
              value={senderOnChainId}
              onChange={e => setSenderOnChainId(e.target.value)}
              required
            />
            <span className="form-hint">
              Hash of your hydra-node's <code>--cardano-signing-key</code> verification
              key — your participant identity in the head. Routing matches this
              against head participants. Derive with:
              <pre className="code-block" style={{ marginTop: '0.4rem' }}>
                cardano-cli address key-hash \{'\n'}
                {'  '}--payment-verification-key-file &lt;your-actor&gt;.vk
              </pre>
            </span>
          </div>
          <div className="form-group">
            <label>Network</label>
            <input type="text" value={network === 'All' ? 'Select a network above' : network} disabled />
            <span className="form-hint">Use the navbar selector to pick Mainnet, Preview, or Preprod</span>
          </div>
          <button type="submit" className="btn btn-primary btn-full" disabled={loading}>
            {loading ? 'Searching...' : 'Find Routes'}
          </button>
        </form>

        {error && !routes && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
          >
            <p>{error}</p>
          </motion.div>
        )}

        {routes && routes.length > 0 && !paymentResult && (
          <div className="routes-list">
            <h3 className="routes-heading">Available Routes ({routes.length})</h3>
            {routes.map((route, i) => (
              <motion.div
                key={route.routeId}
                className="route-card glow-card"
                initial={{ opacity: 0, y: 15 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: i * 0.1, duration: 0.3 }}
              >
                <div className="route-card-header">
                  <span className="route-rank">Route #{i + 1}</span>
                  <span className="route-fee">{(route.totalFee / 1_000_000).toFixed(6)} ADA fee</span>
                </div>
                <div className="route-hops">
                  {route.hops.map((hop, hi) => (
                    <div key={hi} className="route-hop">
                      <span className="hop-index">{hi + 1}</span>
                      <div className="hop-details">
                        <div className="hop-head">
                          <span className="meta-label">Head</span>
                          <code style={{ wordBreak: 'break-all' }}>{hop.headId}</code>
                        </div>
                        <div className="hop-bridge">
                          <span className="meta-label">Bridge</span>
                          <code>{hop.bridgeAddress.slice(0, 16)}...</code>
                        </div>
                        <div className="hop-fee">
                          <span className="meta-label">Fee</span>
                          <span>{(hop.fee / 1_000_000).toFixed(6)} ADA</span>
                        </div>
                      </div>
                      {hi < route.hops.length - 1 && <div className="hop-arrow" />}
                    </div>
                  ))}
                </div>
                <button
                  className="btn btn-primary btn-full"
                  disabled={executing !== null}
                  onClick={() => handleExecute(route.routeId)}
                >
                  {executing === route.routeId ? 'Initiating...' : 'Execute Route'}
                </button>
              </motion.div>
            ))}
          </div>
        )}

        {paymentResult && (
          <motion.div
            className="register-result success"
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
          >
            <h3>Payment Initiated</h3>
            <div className="result-details">
              <div className="result-row">
                <span className="result-label">Payment ID</span>
                <code className="result-value">{paymentResult.routeId}</code>
              </div>
              <div className="result-row">
                <span className="result-label">Status</span>
                <span className="result-value">{paymentResult.status}</span>
              </div>
              <div className="result-row">
                <span className="result-label">Total Fee</span>
                <span className="result-value">{(paymentResult.totalFee / 1_000_000).toFixed(6)} ADA</span>
              </div>
            </div>
            <Link to={`/payments/${paymentResult.routeId}`} className="btn btn-primary btn-full" style={{ marginTop: '1rem', display: 'block', textAlign: 'center' }}>
              Track Payment
            </Link>
          </motion.div>
        )}

        {error && routes && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
          >
            <p>{error}</p>
          </motion.div>
        )}
      </motion.section>
    </div>
  )
}
