import { useState, useEffect, useRef } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'
import { useNetwork, type Network } from '../context/NetworkContext'
import { useWallet } from '../context/WalletContext'

const networks: Network[] = ['All', 'Mainnet', 'Preview', 'Preprod']
const networkPages = new Set(['/explorer', '/routes'])

const ALL_LINKS = [
  { to: '/',          label: 'Home' },
  { to: '/explorer',  label: 'Explorer' },
  { to: '/routes',    label: 'Routes' },
  { to: '/register',  label: 'Register' },
  { to: '/invoice',   label: 'Invoice' },
  { to: '/dashboard', label: 'Dashboard' },
  { to: '/balance',   label: 'Balance' },
  { to: '/setup',     label: 'Setup' },
  { to: '/docs',      label: 'Docs' },
]

// Shown inline in the top bar (excluding brand and wallet button)
const TOP_LINKS = ['/explorer', '/routes']

export default function Navbar() {
  const location = useLocation()
  const [menuOpen, setMenuOpen] = useState(false)
  const { network, setNetwork } = useNetwork()
  const { address, connect, disconnect, available } = useWallet()
  const menuRef = useRef<HTMLDivElement>(null)

  const showNetworkSelector = networkPages.has(location.pathname)

  useEffect(() => { setMenuOpen(false) }, [location.pathname])

  useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => { if (e.key === 'Escape') setMenuOpen(false) }
    window.addEventListener('keydown', handleEsc)
    return () => window.removeEventListener('keydown', handleEsc)
  }, [])

  useEffect(() => {
    const handleClick = (e: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(e.target as Node)) {
        setMenuOpen(false)
      }
    }
    if (menuOpen) document.addEventListener('mousedown', handleClick)
    return () => document.removeEventListener('mousedown', handleClick)
  }, [menuOpen])

  const isActive = (path: string) => location.pathname === path ? 'active' : ''

  const shortAddress = address
    ? `${address.slice(0, 8)}…${address.slice(-4)}`
    : null

  return (
    <nav className="navbar">
      <Link to="/" className="navbar-brand">hydra.registry</Link>

      <div className="navbar-links">
        {ALL_LINKS.filter(l => TOP_LINKS.includes(l.to)).map(l => (
          <Link key={l.to} to={l.to} className={isActive(l.to)}>{l.label}</Link>
        ))}

        {showNetworkSelector && (
          <div className="network-selector">
            {networks.map(n => (
              <button
                key={n}
                className={`network-btn ${network === n ? 'network-active' : ''}`}
                onClick={() => setNetwork(n)}
              >
                {n}
              </button>
            ))}
          </div>
        )}
      </div>

      <div style={{ display: 'flex', alignItems: 'center', gap: '0.75rem' }}>
        {/* Wallet button */}
        {address ? (
          <button className="btn-wallet btn-wallet-connected" onClick={disconnect}>
            {shortAddress}
          </button>
        ) : (
          <button
            className="btn-wallet"
            onClick={() => available.length > 0 ? connect(available[0]) : undefined}
            title={available.length === 0 ? 'No wallet extension detected' : `Connect ${available[0]}`}
            disabled={available.length === 0}
          >
            Connect Wallet
          </button>
        )}

        {/* All-links dropdown */}
        <div className="nav-dropdown-wrap" ref={menuRef}>
          <button
            className={`nav-more-btn ${menuOpen ? 'is-open' : ''}`}
            onClick={() => setMenuOpen(v => !v)}
            aria-label="All pages"
          >
            <span /><span /><span />
          </button>

          <AnimatePresence>
            {menuOpen && (
              <motion.div
                className="nav-dropdown"
                initial={{ opacity: 0, y: -8, scale: 0.97 }}
                animate={{ opacity: 1, y: 0, scale: 1 }}
                exit={{ opacity: 0, y: -8, scale: 0.97 }}
                transition={{ duration: 0.18 }}
              >
                {ALL_LINKS.map(l => (
                  <Link key={l.to} to={l.to} className={`nav-dropdown-item ${isActive(l.to)}`}>
                    {l.label}
                  </Link>
                ))}
                {address && (
                  <>
                    <div className="nav-dropdown-divider" />
                    <button className="nav-dropdown-item nav-dropdown-disconnect" onClick={disconnect}>
                      Disconnect {shortAddress}
                    </button>
                  </>
                )}
              </motion.div>
            )}
          </AnimatePresence>
        </div>
      </div>
    </nav>
  )
}
