import { useEffect, useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import { findRoutes, executeRoute, getRelayGraph, type RouteResponse, type PaymentStatusResponse, type SubgraphResponse } from '../api/client'
import { useNetwork } from '../context/NetworkContext'
import RelayGraph from '../components/RelayGraph'

export default function RouteExplorer() {
  const { network } = useNetwork()
  const [invoiceId, setInvoiceId] = useState('')
  const [senderAddress, setSenderAddress] = useState('')
  const [loading, setLoading] = useState(false)
  const [routes, setRoutes] = useState<RouteResponse[] | null>(null)
  const [executing, setExecuting] = useState<string | null>(null)
  const [paymentResult, setPaymentResult] = useState<PaymentStatusResponse | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [graphData, setGraphData] = useState<SubgraphResponse | null>(null)
  const [graphLoading, setGraphLoading] = useState(false)
  const [graphError, setGraphError] = useState<string | null>(null)

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
        senderAddress,
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
        <h1 className="section-title">Find Payment Route</h1>
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
            <label htmlFor="senderAddress">Your Address (Sender)</label>
            <input
              id="senderAddress"
              type="text"
              placeholder="addr1q..."
              value={senderAddress}
              onChange={e => setSenderAddress(e.target.value)}
              required
            />
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
                          <code>{hop.headId.slice(0, 12)}...</code>
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

      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5, delay: 0.2 }}
      >
        <h2 className="section-title">Head Network Graph</h2>
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

        {graphData && graphData.nodes.length > 0 && (
          <motion.div
            initial={{ opacity: 0, y: 15 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.4 }}
          >
            <RelayGraph nodes={graphData.nodes} edges={graphData.edges} />
          </motion.div>
        )}
      </motion.section>
    </div>
  )
}
