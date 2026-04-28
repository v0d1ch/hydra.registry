import { useState, useEffect } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'
import { useNetwork, type Network } from '../context/NetworkContext'

const networks: Network[] = ['All', 'Mainnet', 'Preview', 'Preprod']

const networkPages = new Set(['/explorer', '/routes'])

export default function Navbar() {
  const location = useLocation()
  const [open, setOpen] = useState(false)
  const { network, setNetwork } = useNetwork()

  // Close menu on route change
  useEffect(() => {
    setOpen(false)
  }, [location.pathname])

  // Close menu on escape
  useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === 'Escape') setOpen(false)
    }
    window.addEventListener('keydown', handleEsc)
    return () => window.removeEventListener('keydown', handleEsc)
  }, [])

  // Prevent body scroll when menu is open
  useEffect(() => {
    document.body.style.overflow = open ? 'hidden' : ''
    return () => { document.body.style.overflow = '' }
  }, [open])

  const isActive = (path: string) => location.pathname === path ? 'active' : ''
  const showNetworkSelector = networkPages.has(location.pathname)

  return (
    <>
      <nav className="navbar">
        <Link to="/" className="navbar-brand">
          hydra.registry
        </Link>

        {/* Desktop links */}
        <div className="navbar-links desktop-only">
          <Link to="/" className={isActive('/')}>Home</Link>
          <Link to="/explorer" className={isActive('/explorer')}>Explorer</Link>
          <Link to="/register" className={isActive('/register')}>Register</Link>
          <Link to="/invoice" className={isActive('/invoice')}>Invoice</Link>
          <Link to="/routes" className={isActive('/routes')}>Routes</Link>
          <Link to="/balance" className={isActive('/balance')}>Balance</Link>
          <Link to="/docs" className={isActive('/docs')}>Docs</Link>
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

        {/* Mobile hamburger */}
        <button
          className={`hamburger mobile-only ${open ? 'is-open' : ''}`}
          onClick={() => setOpen(!open)}
          aria-label="Toggle menu"
          aria-expanded={open}
        >
          <span className="hamburger-line" />
          <span className="hamburger-line" />
          <span className="hamburger-line" />
        </button>
      </nav>

      {/* Mobile menu overlay */}
      <AnimatePresence>
        {open && (
          <motion.div
            className="mobile-menu"
            initial={{ opacity: 0, y: -20 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -20 }}
            transition={{ duration: 0.25 }}
          >
            <div className="mobile-menu-links">
              <Link to="/" className={isActive('/')}>
                <span className="mobile-link-prefix">&gt; </span>Home
              </Link>
              <Link to="/explorer" className={isActive('/explorer')}>
                <span className="mobile-link-prefix">&gt; </span>Explorer
              </Link>
              <Link to="/register" className={isActive('/register')}>
                <span className="mobile-link-prefix">&gt; </span>Register
              </Link>
              <Link to="/invoice" className={isActive('/invoice')}>
                <span className="mobile-link-prefix">&gt; </span>Invoice
              </Link>
              <Link to="/routes" className={isActive('/routes')}>
                <span className="mobile-link-prefix">&gt; </span>Routes
              </Link>
              <Link to="/balance" className={isActive('/balance')}>
                <span className="mobile-link-prefix">&gt; </span>Balance
              </Link>
              <Link to="/docs" className={isActive('/docs')}>
                <span className="mobile-link-prefix">&gt; </span>Docs
              </Link>
              {showNetworkSelector && (
                <div className="mobile-network-selector">
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
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
}
