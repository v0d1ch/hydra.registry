import { useEffect, useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import AnimatedCounter from '../components/AnimatedCounter'
import Typewriter from '../components/Typewriter'
import { getStats, type StatsResponse } from '../api/client'

const title = 'hydra.registry'

const steps = [
  {
    num: '01',
    title: 'Register your Head',
    desc: 'Point hydra.registry at your running Hydra node. We connect via WebSocket and start indexing.',
    icon: '[ ]',
  },
  {
    num: '02',
    title: 'Indexer connects',
    desc: 'We listen for SnapshotConfirmed events and index every UTxO in real-time across all registered heads.',
    icon: '</>',
  },
  {
    num: '03',
    title: 'Wallets query',
    desc: 'Lace, Nami, and Yoroi query our Blockfrost-compatible API as if it were a regular L1 provider.',
    icon: '{ }',
  },
  {
    num: '04',
    title: 'L2 balances appear',
    desc: 'Users see their Hydra funds directly in their favorite wallet. No custom UI needed.',
    icon: ' $ ',
  },
]

const wallets = [
  { name: 'Lace', format: 'Blockfrost', status: 'supported' },
  { name: 'Nami', format: 'Blockfrost', status: 'supported' },
  { name: 'Yoroi', format: 'Yoroi API', status: 'supported' },
  { name: 'Flint', format: 'Blockfrost', status: 'likely' },
  { name: 'Eternl', format: 'Proprietary', status: 'planned' },
  { name: 'VESPR', format: 'Proprietary', status: 'planned' },
]

const cardVariants = {
  hidden: { opacity: 0, y: 40, scale: 0.95 },
  visible: (i: number) => ({
    opacity: 1,
    y: 0,
    scale: 1,
    transition: {
      delay: i * 0.12,
      duration: 0.6,
      ease: [0.25, 0.46, 0.45, 0.94] as [number, number, number, number],
    },
  }),
}

const staggerContainer = {
  hidden: {},
  visible: {
    transition: { staggerChildren: 0.1 },
  },
}

export default function Landing() {
  const [stats, setStats] = useState<StatsResponse | null>(null)

  useEffect(() => {
    getStats().then(setStats).catch(() => {})
  }, [])

  return (
    <div className="landing">
      {/* Hero */}
      <section className="hero">
        <div className="cyber-grid" />
        <div className="scanlines" />
        <div className="hero-orb hero-orb-1" />
        <div className="hero-orb hero-orb-2" />
        <div className="hero-orb hero-orb-3" />

        <motion.div
          className="hero-title glitch"
          data-text={title}
          initial={{ opacity: 0, scale: 0.9 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ duration: 1, ease: [0.25, 0.46, 0.45, 0.94] }}
        >
          {title.split('').map((char, i) => (
            <motion.span
              key={i}
              className="hero-char"
              initial={{ opacity: 0, y: 30, rotateX: -90 }}
              animate={{ opacity: 1, y: 0, rotateX: 0 }}
              transition={{
                delay: i * 0.06,
                duration: 0.5,
                ease: [0.25, 0.46, 0.45, 0.94],
              }}
            >
              {char}
            </motion.span>
          ))}
          <span className="cursor">_</span>
        </motion.div>

        <motion.div
          className="hero-tagline"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.8, duration: 0.6 }}
        >
          <span className="tagline-prefix">&gt; </span>
          <Typewriter text="Unified L2 state for Cardano wallets" delay={40} startDelay={1200} />
        </motion.div>

        <motion.div
          className="hero-subtitle"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 3.2, duration: 0.8 }}
        >
          Index Hydra heads. Serve wallet-compatible APIs. Bridge L1 and L2.
        </motion.div>

        <motion.div
          className="hero-actions"
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 3.8, duration: 0.6 }}
        >
          <Link to="/register" className="btn btn-primary cyber-btn">
            <span className="btn-text">Register a Head</span>
            <span className="btn-shine" />
          </Link>
          <a
            href="https://github.com/v0d1ch/hydra.registry"
            className="btn btn-secondary"
            target="_blank"
            rel="noopener noreferrer"
          >
            View on GitHub
          </a>
        </motion.div>

        <motion.div
          className="scroll-indicator"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 4.5, duration: 0.5 }}
        >
          <div className="scroll-line" />
        </motion.div>
      </section>

      {/* What is it */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          What is hydra.registry?
        </motion.h2>
        <div className="about-grid">
          {[
            {
              icon: '{ }',
              title: 'Index',
              desc: 'A standalone service that connects to Hydra heads via WebSocket, indexes every confirmed snapshot, and stores UTxO state in PostgreSQL.',
            },
            {
              icon: '</>',
              title: 'Serve',
              desc: 'Exposes a REST API compatible with Blockfrost and Yoroi formats so existing wallets can query L2 funds without any modifications.',
            },
            {
              icon: '[ ]',
              title: 'Bridge',
              desc: "Bridges the gap between Hydra's off-chain state and Cardano wallets, making L2 balances visible alongside L1 funds.",
            },
          ].map((card, i) => (
            <motion.div key={card.title} className="about-card glow-card" variants={cardVariants} custom={i + 1}>
              <div className="about-icon mono-icon">{card.icon}</div>
              <h3>{card.title}</h3>
              <p>{card.desc}</p>
            </motion.div>
          ))}
        </div>
      </motion.section>

      {/* How it works */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          How it works
        </motion.h2>
        <div className="steps-grid">
          {steps.map((step, i) => (
            <motion.div
              key={step.num}
              className="step-card glow-card"
              variants={cardVariants}
              custom={i + 1}
            >
              <span className="step-num">{step.num}</span>
              <div className="step-icon mono-icon">{step.icon}</div>
              <h3>{step.title}</h3>
              <p>{step.desc}</p>
              <div className="step-line" />
            </motion.div>
          ))}
        </div>
      </motion.section>

      {/* Wallet compatibility */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          Wallet compatibility
        </motion.h2>
        <div className="wallet-grid">
          {wallets.map((w, i) => (
            <motion.div
              key={w.name}
              className={`wallet-card glow-card wallet-${w.status}`}
              variants={cardVariants}
              custom={i + 1}
            >
              <h3>{w.name}</h3>
              <span className="wallet-format">{w.format}</span>
              <span className={`wallet-badge badge-${w.status}`}>
                {w.status === 'supported'
                  ? 'Supported'
                  : w.status === 'likely'
                    ? 'Likely compatible'
                    : 'Planned'}
              </span>
            </motion.div>
          ))}
        </div>
      </motion.section>

      {/* Live stats */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          Live stats
        </motion.h2>
        {stats ? (
          <div className="stats-grid">
            <AnimatedCounter target={stats.headCount} label="Registered Heads" />
            <AnimatedCounter target={stats.totalUtxos} label="UTxOs Indexed" />
            <AnimatedCounter
              target={Object.values(stats.headsByStatus).reduce((a, b) => a + b, 0)}
              label="Total Connections"
            />
          </div>
        ) : (
          <div className="stats-loading">
            <div className="loading-spinner" />
            <p>Connecting to API...</p>
          </div>
        )}
      </motion.section>
    </div>
  )
}
