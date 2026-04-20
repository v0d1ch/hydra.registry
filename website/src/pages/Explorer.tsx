import { useEffect, useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import { getExplorerHeads, type ExplorerHeadInfo } from '../api/client'
import { useNetwork } from '../context/NetworkContext'

const PAGE_SIZE = 20

const statusColors: Record<string, string> = {
  Open: 'var(--success)',
  Closed: 'var(--error)',
  Finalized: 'var(--text-muted)',
  Initializing: '#f0c040',
}

export default function Explorer() {
  const { network } = useNetwork()
  const [heads, setHeads] = useState<ExplorerHeadInfo[]>([])
  const [page, setPage] = useState(1)
  const [statusFilter, setStatusFilter] = useState<string>('')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    setLoading(true)
    setError(null)
    getExplorerHeads(PAGE_SIZE, page, statusFilter || undefined, network === 'All' ? undefined : network)
      .then(setHeads)
      .catch(e => setError(e.message))
      .finally(() => setLoading(false))
  }, [page, statusFilter, network])

  return (
    <div className="explorer-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">On-Chain Head Explorer</h1>
        <p className="explorer-desc">
          Heads discovered on-chain by the hydra-explorer sidecar. These are live Hydra heads
          across Cardano {network === 'All' ? 'networks' : network}.
        </p>

        <div className="explorer-filters">
          <div className="filter-group">
            <label>Status</label>
            <select value={statusFilter} onChange={e => { setStatusFilter(e.target.value); setPage(1) }}>
              <option value="">All</option>
              <option value="Initializing">Initializing</option>
              <option value="Open">Open</option>
              <option value="Closed">Closed</option>
              <option value="Finalized">Finalized</option>
            </select>
          </div>
        </div>

        {loading && (
          <div className="stats-loading">
            <div className="loading-spinner" />
            <p>Loading heads...</p>
          </div>
        )}

        {error && (
          <div className="register-result error">
            <p>{error}</p>
          </div>
        )}

        {!loading && !error && heads.length === 0 && (
          <div className="explorer-empty">
            <p>No heads found matching your filters.</p>
          </div>
        )}

        {!loading && !error && heads.length > 0 && (
          <div className="explorer-grid">
            {heads.map((head, i) => (
              <motion.div
                key={head.headId}
                className="explorer-card glow-card"
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: i * 0.05, duration: 0.3 }}
              >
                <div className="explorer-card-header">
                  <span
                    className="status-dot"
                    style={{ background: statusColors[head.status] ?? 'var(--text-muted)' }}
                  />
                  <span className="explorer-status">{head.status}</span>
                  <span className="explorer-network">{head.network}</span>
                </div>
                <div className="explorer-card-id">
                  <code>{head.headId.slice(0, 16)}...{head.headId.slice(-8)}</code>
                </div>
                <div className="explorer-card-meta">
                  <div className="meta-row">
                    <span className="meta-label">Version</span>
                    <span className="meta-value">{head.version}</span>
                  </div>
                  {head.snapshotNumber !== null && (
                    <div className="meta-row">
                      <span className="meta-label">Snapshots</span>
                      <span className="meta-value">{head.snapshotNumber}</span>
                    </div>
                  )}
                  {head.contestationPeriod !== null && (
                    <div className="meta-row">
                      <span className="meta-label">Contestation</span>
                      <span className="meta-value">{head.contestationPeriod}s</span>
                    </div>
                  )}
                  {head.blockNo !== null && (
                    <div className="meta-row">
                      <span className="meta-label">Block</span>
                      <span className="meta-value">{head.blockNo}</span>
                    </div>
                  )}
                  <div className="meta-row">
                    <span className="meta-label">First seen</span>
                    <span className="meta-value">{new Date(head.firstSeenAt).toLocaleDateString()}</span>
                  </div>
                  {head.registered && (
                    <div className="meta-row">
                      <span className="meta-label badge-registered">Registered</span>
                    </div>
                  )}
                </div>
              </motion.div>
            ))}
          </div>
        )}

        {!loading && heads.length >= PAGE_SIZE && (
          <div className="pagination">
            <button
              className="btn btn-secondary"
              disabled={page <= 1}
              onClick={() => setPage(p => p - 1)}
            >
              Previous
            </button>
            <span className="page-num">Page {page}</span>
            <button
              className="btn btn-secondary"
              disabled={heads.length < PAGE_SIZE}
              onClick={() => setPage(p => p + 1)}
            >
              Next
            </button>
          </div>
        )}
      </motion.section>
    </div>
  )
}
