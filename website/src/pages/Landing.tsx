import { useEffect, useState } from 'react'
import { motion } from 'framer-motion'
import { Link } from 'react-router-dom'
import AnimatedCounter from '../components/AnimatedCounter'
import Typewriter from '../components/Typewriter'
import { getStats, type StatsResponse } from '../api/client'

const title = 'hydra.registry'

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
          <Typewriter text="Pay anyone across Hydra heads" delay={40} startDelay={1200} />
        </motion.div>

        <motion.div
          className="hero-subtitle"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 3.2, duration: 0.8 }}
        >
          The relay finds the cheapest route between Hydra heads and moves your funds via HTLC contracts.
        </motion.div>

        <motion.div
          className="hero-actions"
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 3.8, duration: 0.6 }}
        >
          <Link to="/routes" className="btn btn-primary cyber-btn">
            <span className="btn-text">Send a Payment</span>
            <span className="btn-shine" />
          </Link>
          <Link to="/explorer" className="btn btn-secondary">
            Explore Heads
          </Link>
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
          {[
            {
              num: '01',
              icon: '#',
              title: 'Receiver creates invoice',
              desc: 'The receiver generates a secret offline, hashes it, and publishes an invoice with the amount and hash.',
            },
            {
              num: '02',
              icon: '~',
              title: 'Relay finds the route',
              desc: 'The sender enters the invoice ID. The relay discovers the cheapest path through bridge operators across heads.',
            },
            {
              num: '03',
              icon: '!',
              title: 'HTLC locks funds',
              desc: 'Funds are locked hop-by-hop in HTLC contracts along the route. Each bridge operator holds funds until the secret is revealed.',
            },
            {
              num: '04',
              icon: '$',
              title: 'Secret claims payment',
              desc: 'The receiver reveals the secret to claim their funds. Bridge operators use it to unlock their portion in sequence.',
            },
          ].map((step, i) => (
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

      {/* Why use it */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          Why hydra.registry?
        </motion.h2>
        <div className="about-grid">
          {[
            {
              icon: '~',
              title: 'Cheapest path',
              desc: 'Dijkstra pathfinding weighted by bridge operator fees. Up to 3 route options so you can pick the best deal.',
            },
            {
              icon: '!',
              title: 'Trustless',
              desc: 'HTLC contracts guarantee atomicity. Either the full payment completes or everyone gets refunded. No middleman holds your funds.',
            },
            {
              icon: '#',
              title: 'Multi-hop',
              desc: 'Payments can traverse multiple heads through bridge operators. Each hop adds a small fee but opens up more routes.',
            },
            {
              icon: '@',
              title: 'Wallet-ready L2 balances',
              desc: 'Wallets like Lace and Yoroi can query Hydra head balances through our Blockfrost-compatible API. See your L2 funds alongside L1.',
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

      {/* CTA */}
      <motion.section
        className="section"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true, margin: '-100px' }}
        variants={staggerContainer}
      >
        <motion.h2 className="section-title" variants={cardVariants} custom={0}>
          Get started
        </motion.h2>
        <div className="about-grid">
          {[
            {
              icon: '>',
              title: 'Send a payment',
              desc: 'Have an invoice ID? Find the cheapest route and pay in seconds.',
              link: '/routes',
              linkText: 'Find Routes',
            },
            {
              icon: '<',
              title: 'Receive a payment',
              desc: 'Generate a secret, create an invoice, and share the ID with the sender.',
              link: '/invoice',
              linkText: 'Create Invoice',
            },
            {
              icon: '$',
              title: 'Earn fees as a bridge',
              desc: 'Register your Hydra head as a bridge operator and earn fees for every payment relayed through it.',
              link: '/register',
              linkText: 'Register Head',
            },
          ].map((card, i) => (
            <motion.div key={card.title} className="about-card glow-card" variants={cardVariants} custom={i + 1}>
              <div className="about-icon mono-icon">{card.icon}</div>
              <h3>{card.title}</h3>
              <p>{card.desc}</p>
              <Link to={card.link} className="card-link">{card.linkText}</Link>
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
          Network
        </motion.h2>
        {stats ? (
          <div className="stats-grid">
            <AnimatedCounter target={stats.headCount} label="Registered Heads" />
            <AnimatedCounter target={stats.explorerHeadCount} label="On-chain Heads" />
            <AnimatedCounter target={stats.totalUtxos} label="UTxOs Indexed" />
            <AnimatedCounter
              target={Object.values(stats.headsByStatus).reduce((a, b) => a + b, 0)}
              label="Active Connections"
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
