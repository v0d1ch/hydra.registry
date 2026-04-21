import { useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import {
  getHeadsByAddress,
  getAddressUtxos,
  getExplorerHead,
  getHeadParticipants,
  type ParticipantHeadInfo,
  type UtxoResponse,
  type ExplorerHeadInfo,
} from '../api/client'

interface HeadBalance {
  headId: string
  headStatus: string
  network: string
  lovelace: number
  committedLovelace: number
  tokens: { unit: string; quantity: string }[]
  utxoCount: number
  source: 'live' | 'committed'
}

function groupByHead(participants: ParticipantHeadInfo[], utxos: UtxoResponse[]): HeadBalance[] {
  const headMap = new Map<string, HeadBalance>()

  for (const p of participants) {
    headMap.set(p.headId, {
      headId: p.headId,
      headStatus: p.headStatus,
      network: p.network,
      lovelace: 0,
      committedLovelace: p.committedLovelace,
      tokens: [],
      utxoCount: 0,
      source: 'committed',
    })
  }

  for (const u of utxos) {
    let entry = headMap.get(u.head_id)
    if (!entry) {
      entry = {
        headId: u.head_id,
        headStatus: 'Unknown',
        network: '',
        lovelace: 0,
        committedLovelace: 0,
        tokens: [],
        utxoCount: 0,
        source: 'live',
      }
      headMap.set(u.head_id, entry)
    }
    entry.source = 'live'
    entry.utxoCount++
    for (const a of u.amount) {
      if (a.unit === 'lovelace') {
        entry.lovelace += parseInt(a.quantity, 10)
      } else {
        const existing = entry.tokens.find(t => t.unit === a.unit)
        if (existing) {
          existing.quantity = String(parseInt(existing.quantity, 10) + parseInt(a.quantity, 10))
        } else {
          entry.tokens.push({ ...a })
        }
      }
    }
  }

  return Array.from(headMap.values())
}

const statusColors: Record<string, string> = {
  Open: 'var(--success)',
  Closed: 'var(--error)',
  Finalized: 'var(--text-muted)',
  Initializing: '#f0c040',
}

export default function Balance() {
  const [address, setAddress] = useState('')
  const [loading, setLoading] = useState(false)
  const [balances, setBalances] = useState<HeadBalance[] | null>(null)
  const [headDetail, setHeadDetail] = useState<ExplorerHeadInfo | null>(null)
  const [headParticipants, setHeadParticipants] = useState<ParticipantHeadInfo[]>([])
  const [error, setError] = useState<string | null>(null)

  const isHeadId = (input: string) => /^[0-9a-fA-F]{56}$/.test(input.trim())

  const handleLookup = async (e: React.FormEvent) => {
    e.preventDefault()
    const input = address.trim()
    if (!input) return
    setLoading(true)
    setError(null)
    setBalances(null)
    setHeadDetail(null)
    setHeadParticipants([])

    try {
      if (isHeadId(input)) {
        // Head ID lookup — show head info + participants
        const [found, participants] = await Promise.all([
          getExplorerHead(input).catch(() => null),
          getHeadParticipants(input).catch(() => [] as ParticipantHeadInfo[]),
        ])
        setHeadDetail(found)
        setHeadParticipants(participants)
        setBalances([])
      } else {
        // Address lookup
        const [participants, utxos] = await Promise.all([
          getHeadsByAddress(input).catch(() => [] as ParticipantHeadInfo[]),
          getAddressUtxos(input).catch(() => [] as UtxoResponse[]),
        ])
        setBalances(groupByHead(participants, utxos))
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch data')
    } finally {
      setLoading(false)
    }
  }

  const totalLovelace = balances?.reduce((sum, b) =>
    sum + (b.source === 'live' ? b.lovelace : b.committedLovelace), 0) ?? 0

  return (
    <div className="balance-page">
      <motion.section
        className="section"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        <h1 className="section-title">L2 Balance Lookup</h1>
        <p className="register-desc">
          Enter a Cardano address to see your balances across Hydra heads,
          or paste a head ID to inspect a specific head.
        </p>

        <form className="register-form" onSubmit={handleLookup}>
          <div className="form-group">
            <label htmlFor="address">Address or Head ID</label>
            <input
              id="address"
              type="text"
              placeholder="addr1q... or head ID (56 hex chars)"
              value={address}
              onChange={e => setAddress(e.target.value)}
              required
            />
          </div>
          <button type="submit" className="btn btn-primary btn-full" disabled={loading}>
            {loading ? 'Looking up...' : 'Lookup'}
          </button>
        </form>

        {loading && (
          <div className="stats-loading">
            <div className="loading-spinner" />
            <p>Querying Hydra heads...</p>
          </div>
        )}

        {error && (
          <motion.div
            className="register-result error"
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
          >
            <h3>Error</h3>
            <p>{error}</p>
          </motion.div>
        )}

        {/* Head ID detail view */}
        {headDetail && !loading && (
          <motion.div
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3 }}
          >
            <div className="balance-card glow-card" style={{ marginTop: '2rem' }}>
              <div className="explorer-card-header">
                <span
                  className="status-dot"
                  style={{ background: statusColors[headDetail.status] ?? 'var(--text-muted)' }}
                />
                <span className="explorer-status">{headDetail.status}</span>
                <span className="explorer-network">{headDetail.network}</span>
                {headDetail.registered && <span className="badge-registered">Registered</span>}
                {headDetail.htlcEnabled && <span className="badge-htlc">HTLC</span>}
              </div>
              <div className="explorer-card-id" style={{ marginTop: '0.75rem' }}>
                <code className="result-value-mono">{headDetail.headId}</code>
              </div>
              <div className="result-details" style={{ marginTop: '1rem' }}>
                <div className="result-row">
                  <span className="result-label">Version</span>
                  <span className="result-value">{headDetail.version}</span>
                </div>
                {headDetail.snapshotNumber !== null && (
                  <div className="result-row">
                    <span className="result-label">Snapshots</span>
                    <span className="result-value">{headDetail.snapshotNumber}</span>
                  </div>
                )}
                {headDetail.contestationPeriod !== null && (
                  <div className="result-row">
                    <span className="result-label">Contestation period</span>
                    <span className="result-value">{headDetail.contestationPeriod}s</span>
                  </div>
                )}
                {headDetail.blockNo !== null && (
                  <div className="result-row">
                    <span className="result-label">Block</span>
                    <span className="result-value">{headDetail.blockNo}</span>
                  </div>
                )}
                <div className="result-row">
                  <span className="result-label">First seen</span>
                  <span className="result-value">{new Date(headDetail.firstSeenAt).toLocaleString()}</span>
                </div>
                <div className="result-row">
                  <span className="result-label">Last updated</span>
                  <span className="result-value">{new Date(headDetail.lastUpdatedAt).toLocaleString()}</span>
                </div>
              </div>

              {headParticipants.length > 0 && (
                <div className="head-participants-section">
                  <div className="head-tvl">
                    <span className="head-tvl-label">Total Value Locked</span>
                    <span className="head-tvl-amount">
                      {(headParticipants.reduce((sum, p) => sum + p.committedLovelace, 0) / 1_000_000)
                        .toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 6 })} ADA
                    </span>
                  </div>
                  <h4 className="head-participants-title">
                    Participants ({headParticipants.length})
                  </h4>
                  <div className="head-participants-list">
                    {headParticipants.map((p, idx) => (
                      <div key={idx} className="head-participant-row">
                        <code className="head-participant-address">{p.address}</code>
                        <span className="head-participant-committed">
                          {(p.committedLovelace / 1_000_000).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 6 })} ADA
                        </span>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
          </motion.div>
        )}

        {/* Head ID not found */}
        {!headDetail && balances !== null && balances.length === 0 && isHeadId(address.trim()) && !loading && (
          <div className="explorer-empty" style={{ marginTop: '2rem' }}>
            <p>Head not found in the explorer. It may not have been discovered on-chain yet.</p>
            <p className="balance-empty-hint">
              Try browsing the <Link to="/explorer">Explorer</Link> to see all known heads.
            </p>
          </div>
        )}

        {/* Address balance results */}
        {balances !== null && !isHeadId(address.trim()) && !loading && (
          <motion.div
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3 }}
          >
            {balances.length === 0 ? (
              <div className="explorer-empty">
                <p>No Hydra head data found for this address.</p>
                <p className="balance-empty-hint">
                  This address was not found as a participant in any discovered head.
                  If you know your head ID, try pasting it directly.
                </p>
              </div>
            ) : (
              <>
                <div className="balance-summary glow-card">
                  <div className="balance-total-label">Total L2 Balance</div>
                  <div className="balance-total-amount">
                    {(totalLovelace / 1_000_000).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 6 })} ADA
                  </div>
                  <div className="balance-total-sub">
                    across {balances.length} head{balances.length !== 1 && 's'}
                  </div>
                </div>

                <div className="balance-grid">
                  {balances.map((b, i) => {
                    const displayLovelace = b.source === 'live' ? b.lovelace : b.committedLovelace
                    return (
                      <motion.div
                        key={b.headId}
                        className="balance-card glow-card"
                        initial={{ opacity: 0, y: 20 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ delay: i * 0.05, duration: 0.3 }}
                      >
                        <div className="explorer-card-header">
                          <span
                            className="status-dot"
                            style={{ background: statusColors[b.headStatus] ?? 'var(--text-muted)' }}
                          />
                          <span className="explorer-status">{b.headStatus}</span>
                          {b.network && <span className="explorer-network">{b.network}</span>}
                        </div>
                        <div className="explorer-card-id">
                          <code>{b.headId.slice(0, 16)}...{b.headId.slice(-8)}</code>
                        </div>
                        <div className="balance-amount">
                          {(displayLovelace / 1_000_000).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 6 })} ADA
                        </div>
                        <div className="balance-meta">
                          {b.source === 'live' ? (
                            <>
                              <span>{b.utxoCount} UTxO{b.utxoCount !== 1 && 's'}</span>
                              {b.tokens.length > 0 && (
                                <span>{b.tokens.length} token{b.tokens.length !== 1 && 's'}</span>
                              )}
                              <span className="balance-source-live">Live</span>
                            </>
                          ) : (
                            <span className="balance-source-committed">Committed</span>
                          )}
                        </div>
                        {b.source === 'live' && b.tokens.length > 0 && (
                          <div className="balance-tokens">
                            {b.tokens.map(t => (
                              <div key={t.unit} className="balance-token-row">
                                <code className="balance-token-unit">{t.unit.slice(0, 12)}...{t.unit.slice(-8)}</code>
                                <span>{t.quantity}</span>
                              </div>
                            ))}
                          </div>
                        )}
                      </motion.div>
                    )
                  })}
                </div>
              </>
            )}
          </motion.div>
        )}
      </motion.section>
    </div>
  )
}
