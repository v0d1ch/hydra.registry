import { useEffect, useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { getExplorerHeads, getExplorerStats, type ExplorerHeadInfo, type ExplorerStatsResponse } from '../api/client'
import { useNetwork } from '../context/NetworkContext'
import AnimatedCounter from '../components/AnimatedCounter'

const PAGE_SIZE = 20

const statusColors: Record<string, string> = {
  Open: 'var(--success)',
  Closed: 'var(--error)',
  Finalized: 'var(--text-muted)',
  Initializing: '#f0c040',
}

function CopyButton({ text }: { text: string }) {
  const [copied, setCopied] = useState(false)

  const handleCopy = (e: React.MouseEvent) => {
    e.stopPropagation()
    navigator.clipboard.writeText(text).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 1500)
    })
  }

  return (
    <button className="copy-btn" onClick={handleCopy} title="Copy head ID">
      {copied ? 'Copied' : 'Copy'}
    </button>
  )
}

export default function Explorer() {
  const { network } = useNetwork()
  const [heads, setHeads] = useState<ExplorerHeadInfo[]>([])
  const [page, setPage] = useState(1)
  const [statusFilter, setStatusFilter] = useState<string>('')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [expandedId, setExpandedId] = useState<string | null>(null)
  const [stats, setStats] = useState<ExplorerStatsResponse | null>(null)

  useEffect(() => {
    getExplorerStats(statusFilter || undefined, network === 'All' ? undefined : network)
      .then(setStats)
      .catch(() => {})
  }, [statusFilter, network])

  useEffect(() => {
    setLoading(true)
    setError(null)
    getExplorerHeads(PAGE_SIZE, page, statusFilter || undefined, network === 'All' ? undefined : network)
      .then(setHeads)
      .catch(e => setError(e.message))
      .finally(() => setLoading(false))
  }, [page, statusFilter, network])

  const toggleExpand = (headId: string) => {
    setExpandedId(prev => prev === headId ? null : headId)
  }

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

        {stats && (
          <div className="explorer-stats">
            <AnimatedCounter target={stats.explorerHeadCount} label="On-chain Heads" />
            <AnimatedCounter target={stats.uniqueParticipants} label="Unique Participants" />
            <AnimatedCounter
              target={Math.floor(stats.totalCommittedLovelace / 1_000_000)}
              label="Total Committed (ADA)"
            />
          </div>
        )}

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
                className={`explorer-card glow-card ${expandedId === head.headId ? 'explorer-card-expanded' : ''}`}
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: i * 0.05, duration: 0.3 }}
                onClick={() => toggleExpand(head.headId)}
                style={{ cursor: 'pointer' }}
              >
                <div className="explorer-card-header">
                  <span
                    className="status-dot"
                    style={{ background: statusColors[head.status] ?? 'var(--text-muted)' }}
                  />
                  <span className="explorer-status">{head.status}</span>
                  <span className="explorer-network">{head.network}</span>
                  {head.registered && <span className="badge-registered">Registered</span>}
                  {head.htlcEnabled && <span className="badge-htlc">HTLC</span>}
                </div>
                <div className="explorer-card-id">
                  <code>{head.headId.slice(0, 16)}...{head.headId.slice(-8)}</code>
                  <CopyButton text={head.headId} />
                </div>
                <div className="explorer-card-meta">
                  <div className="meta-row">
                    <span className="meta-label">Version</span>
                    <span className="meta-value">{head.version}</span>
                  </div>
                  {head.snapshotNumber !== null && head.snapshotNumber > 0 && (
                    <div className="meta-row">
                      <span className="meta-label">Snapshots</span>
                      <span className="meta-value">{head.snapshotNumber}</span>
                    </div>
                  )}
                </div>

                <AnimatePresence>
                  {expandedId === head.headId && (
                    <motion.div
                      className="explorer-card-detail"
                      initial={{ opacity: 0, height: 0 }}
                      animate={{ opacity: 1, height: 'auto' }}
                      exit={{ opacity: 0, height: 0 }}
                      transition={{ duration: 0.2 }}
                    >
                      <div className="detail-divider" />
                      {head.contestationPeriod !== null && (
                        <div className="meta-row">
                          <span className="meta-label">Contestation period</span>
                          <span className="meta-value">{head.contestationPeriod}s</span>
                        </div>
                      )}
                      {head.contestations !== null && head.contestations > 0 && (
                        <div className="meta-row">
                          <span className="meta-label">Contestations</span>
                          <span className="meta-value">{head.contestations}</span>
                        </div>
                      )}
                      {head.contestationDeadline && (
                        <div className="meta-row">
                          <span className="meta-label">Contestation deadline</span>
                          <span className="meta-value">{new Date(head.contestationDeadline).toLocaleString()}</span>
                        </div>
                      )}
                      {head.blockNo !== null && (
                        <div className="meta-row">
                          <span className="meta-label">Block</span>
                          <span className="meta-value">{head.blockNo}</span>
                        </div>
                      )}
                      <div className="meta-row">
                        <span className="meta-label">Network magic</span>
                        <span className="meta-value">{head.networkMagic}</span>
                      </div>
                      <div className="meta-row">
                        <span className="meta-label">Last updated</span>
                        <span className="meta-value">{new Date(head.lastUpdatedAt).toLocaleString()}</span>
                      </div>
                      {head.seedTxIn && (
                        <div className="meta-row">
                          <span className="meta-label">Seed TxIn</span>
                          <code className="meta-value meta-value-mono">{head.seedTxIn}</code>
                        </div>
                      )}
                    </motion.div>
                  )}
                </AnimatePresence>
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
