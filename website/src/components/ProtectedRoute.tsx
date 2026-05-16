import { useWallet } from '../context/WalletContext'
import SyncBanner from './SyncBanner'

export default function ProtectedRoute({ children }: { children: React.ReactNode }) {
  const { address } = useWallet()

  if (!address) {
    return (
      <div className="section" style={{ textAlign: 'center', paddingTop: '5rem' }}>
        <p style={{ fontSize: '1.1rem', color: 'var(--text-muted)', marginBottom: '0.5rem' }}>
          Connect your wallet to access this page.
        </p>
        <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)' }}>
          Use the "Connect Wallet" button in the top bar.
        </p>
      </div>
    )
  }

  return (
    <>
      <SyncBanner />
      {children}
    </>
  )
}
