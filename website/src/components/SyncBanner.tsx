import { useState, useEffect } from 'react'
import { getHealth, type HealthResponse } from '../api/client'

export default function SyncBanner() {
  const [health, setHealth] = useState<HealthResponse | null>(null)

  useEffect(() => {
    getHealth().then(setHealth).catch(() => {})
  }, [])

  if (!health) return null

  const syncProgress = health.nodeSyncProgress
  const isSyncing = syncProgress !== null && syncProgress !== undefined && syncProgress < 100

  if (!isSyncing) return null

  return (
    <div style={{
      margin: '0 0 1.5rem 0',
      padding: '0.75rem 1rem',
      background: 'rgba(240, 192, 64, 0.08)',
      border: '1px solid rgba(240, 192, 64, 0.35)',
      borderRadius: '8px',
      fontSize: '0.85rem',
      color: '#f0c040',
    }}>
      Cardano node syncing - {syncProgress?.toFixed(2)}% complete. On-chain features (routes, HTLC transactions) will be available once sync reaches 100%.
    </div>
  )
}
