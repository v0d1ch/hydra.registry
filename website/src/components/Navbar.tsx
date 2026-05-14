import { useState, useEffect, useRef } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'
import { useNetwork, type Network } from '../context/NetworkContext'
import { useWallet } from '../context/WalletContext'

const networks: Network[] = ['All', 'Mainnet', 'Preview', 'Preprod']
const networkPages = new Set(['/explorer', '/routes'])

const TOP_LINKS = [
  { to: '/explorer', label: 'Explorer' },
  { to: '/routes',   label: 'Routes' },
  { to: '/docs',     label: 'Docs' },
]

const DROPDOWN_LINKS = [
  { to: '/',          label: 'Home' },
  { to: '/register',  label: 'Register' },
  { to: '/invoice',   label: 'Invoice' },
  { to: '/dashboard', label: 'Dashboard' },
  { to: '/balance',   label: 'Balance' },
  { to: '/setup',     label: 'Setup' },
]

function getAvailableWallets(): string[] {
  if (typeof window === 'undefined' || !window.cardano) return []
  return Object.keys(window.cardano).filter(k => {
    try { return typeof (window.cardano![k] as { enable?: unknown })?.enable === 'function' } catch { return false }
  })
}

export default function Navbar() {
  const location = useLocation()
  const [menuOpen, setMenuOpen] = useState(false)
  const [walletPickerOpen, setWalletPickerOpen] = useState(false)
  const [connectError, setConnectError] = useState<string | null>(null)
  const { network, setNetwork } = useNetwork()
  const { address, connect, disconnect } = useWallet()
  const menuRef = useRef<HTMLDivElement>(null)
  const walletRef = useRef<HTMLDivElement>(null)

  const showNetworkSelector = networkPages.has(location.pathname)

  useEffect(() => { setMenuOpen(false); setWalletPickerOpen(false) }, [location.pathname])

  useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === 'Escape') { setMenuOpen(false); setWalletPickerOpen(false) }
    }
    window.addEventListener('keydown', handleEsc)
    return () => window.removeEventListener('keydown', handleEsc)
  }, [])

  useEffect(() => {
    const handleClick = (e: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(e.target as Node)) setMenuOpen(false)
      if (walletRef.current && !walletRef.current.contains(e.target as Node)) {
        setWalletPickerOpen(false); setConnectError(null)
      }
    }
    document.addEventListener('mousedown', handleClick)
    return () => document.removeEventListener('mousedown', handleClick)
  }, [])

  const isActive = (path: string) => location.pathname === path ? 'active' : ''

  const shortAddress = address
    ? `${address.slice(0, 8)}…${address.slice(-4)}`
    : null

  const handleConnectClick = () => {
    setConnectError(null)
    const wallets = getAvailableWallets()
    if (wallets.length === 0) {
      setConnectError('No wallet extension detected')
      setWalletPickerOpen(true)
      return
    }
    if (wallets.length === 1) {
      connect(wallets[0]).catch(e => {
        setConnectError(e instanceof Error ? e.message : 'Connection failed')
        setWalletPickerOpen(true)
      })
      return
    }
    setWalletPickerOpen(v => !v)
  }

  const handlePickWallet = (name: string) => {
    setConnectError(null)
    connect(name)
      .then(() => setWalletPickerOpen(false))
      .catch(e => setConnectError(e instanceof Error ? e.message : 'Connection failed'))
  }

  return (
    <nav className="navbar">
      <Link to="/" className="navbar-brand">hydra.registry</Link>

      {/* Desktop: inline nav links */}
      <div className="navbar-links navbar-desktop">
        {TOP_LINKS.map(l => (
          <Link key={l.to} to={l.to} className={isActive(l.to)}>{l.label}</Link>
        ))}
        {showNetworkSelector && (
          <div className="network-selector">
            {networks.map(n => (
              <button key={n} className={`network-btn ${network === n ? 'network-active' : ''}`} onClick={() => setNetwork(n)}>{n}</button>
            ))}
          </div>
        )}
      </div>

      {/* Right side: wallet button (always rendered once) + desktop dropdown + mobile hamburger */}
      <div className="navbar-right">
        {/* Wallet button — single instance, always visible */}
        <div className="nav-dropdown-wrap" ref={walletRef}>
          {address ? (
            <button className="btn-wallet btn-wallet-connected" onClick={disconnect}>{shortAddress}</button>
          ) : (
            <button className="btn-wallet" onClick={handleConnectClick}>Connect Wallet</button>
          )}
          <AnimatePresence>
            {walletPickerOpen && !address && (
              <motion.div className="nav-dropdown" style={{ minWidth: '180px' }}
                initial={{ opacity: 0, y: -8, scale: 0.97 }} animate={{ opacity: 1, y: 0, scale: 1 }}
                exit={{ opacity: 0, y: -8, scale: 0.97 }} transition={{ duration: 0.18 }}
              >
                {connectError ? (
                  <div style={{ padding: '0.6rem 0.75rem', fontSize: '0.8rem', color: 'var(--error, #f87171)' }}>{connectError}</div>
                ) : (
                  getAvailableWallets().map(name => (
                    <button key={name} className="nav-dropdown-item" onClick={() => handlePickWallet(name)} style={{ textTransform: 'capitalize' }}>{name}</button>
                  ))
                )}
              </motion.div>
            )}
          </AnimatePresence>
        </div>

        {/* Desktop: pages dropdown (logged-in only) */}
        {address && (
          <div className="nav-dropdown-wrap navbar-desktop" ref={menuRef}>
            <button className={`nav-more-btn ${menuOpen ? 'is-open' : ''}`} onClick={() => setMenuOpen(v => !v)} aria-label="All pages">
              <span /><span /><span />
            </button>
            <AnimatePresence>
              {menuOpen && (
                <motion.div className="nav-dropdown"
                  initial={{ opacity: 0, y: -8, scale: 0.97 }} animate={{ opacity: 1, y: 0, scale: 1 }}
                  exit={{ opacity: 0, y: -8, scale: 0.97 }} transition={{ duration: 0.18 }}
                >
                  {DROPDOWN_LINKS.map(l => (
                    <Link key={l.to} to={l.to} className={`nav-dropdown-item ${isActive(l.to)}`}>{l.label}</Link>
                  ))}
                  <div className="nav-dropdown-divider" />
                  <button className="nav-dropdown-item nav-dropdown-disconnect" onClick={disconnect}>Disconnect {shortAddress}</button>
                </motion.div>
              )}
            </AnimatePresence>
          </div>
        )}

        {/* Mobile: hamburger with all nav links */}
        <div className="nav-dropdown-wrap navbar-mobile" ref={menuRef}>
          <button className={`nav-more-btn ${menuOpen ? 'is-open' : ''}`} onClick={() => setMenuOpen(v => !v)} aria-label="Menu">
            <span /><span /><span />
          </button>
          <AnimatePresence>
            {menuOpen && (
              <motion.div className="nav-dropdown nav-dropdown-mobile"
                initial={{ opacity: 0, y: -8, scale: 0.97 }} animate={{ opacity: 1, y: 0, scale: 1 }}
                exit={{ opacity: 0, y: -8, scale: 0.97 }} transition={{ duration: 0.18 }}
              >
                {TOP_LINKS.map(l => (
                  <Link key={l.to} to={l.to} className={`nav-dropdown-item ${isActive(l.to)}`}>{l.label}</Link>
                ))}
                {address && (
                  <>
                    <div className="nav-dropdown-divider" />
                    {DROPDOWN_LINKS.map(l => (
                      <Link key={l.to} to={l.to} className={`nav-dropdown-item ${isActive(l.to)}`}>{l.label}</Link>
                    ))}
                    <div className="nav-dropdown-divider" />
                    <button className="nav-dropdown-item nav-dropdown-disconnect" onClick={() => { disconnect(); setMenuOpen(false) }}>
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
