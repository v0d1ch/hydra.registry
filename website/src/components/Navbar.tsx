import { useState, useEffect } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'

export default function Navbar() {
  const location = useLocation()
  const [open, setOpen] = useState(false)

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

  return (
    <>
      <nav className="navbar">
        <Link to="/" className="navbar-brand">
          hydra.registry
        </Link>

        {/* Desktop links */}
        <div className="navbar-links desktop-only">
          <Link to="/" className={location.pathname === '/' ? 'active' : ''}>
            Home
          </Link>
          <Link to="/register" className={location.pathname === '/register' ? 'active' : ''}>
            Register Head
          </Link>
          <a href="https://github.com/v0d1ch/hydra.registry" target="_blank" rel="noopener noreferrer">
            GitHub
          </a>
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
              <Link to="/" className={location.pathname === '/' ? 'active' : ''}>
                <span className="mobile-link-prefix">&gt; </span>Home
              </Link>
              <Link to="/register" className={location.pathname === '/register' ? 'active' : ''}>
                <span className="mobile-link-prefix">&gt; </span>Register Head
              </Link>
              <a href="https://github.com/v0d1ch/hydra.registry" target="_blank" rel="noopener noreferrer">
                <span className="mobile-link-prefix">&gt; </span>GitHub
              </a>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
}
