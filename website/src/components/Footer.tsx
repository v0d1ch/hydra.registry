export default function Footer() {
  return (
    <footer className="footer">
      <div className="footer-content">
        <div className="footer-brand">
          <span className="footer-logo">hydra.registry</span>
          <p>Unified L2 state for Cardano wallets</p>
        </div>
        <div className="footer-links">
          <div className="footer-col">
            <h4>Product</h4>
            <a href="https://github.com/v0d1ch/hydra.registry" target="_blank" rel="noopener noreferrer">
              GitHub
            </a>
            <a href="/api/v1/health">API Health</a>
            <a href="/api/v1/stats">API Stats</a>
          </div>
          <div className="footer-col">
            <h4>Ecosystem</h4>
            <a href="https://hydra.family" target="_blank" rel="noopener noreferrer">
              Hydra Protocol
            </a>
            <a href="https://explorer.hydra.family" target="_blank" rel="noopener noreferrer">
              Hydra Explorer
            </a>
            <a href="https://cardano.org" target="_blank" rel="noopener noreferrer">
              Cardano
            </a>
          </div>
          <div className="footer-col">
            <h4>Wallets</h4>
            <a href="https://www.lace.io" target="_blank" rel="noopener noreferrer">Lace</a>
            <a href="https://namiwallet.io" target="_blank" rel="noopener noreferrer">Nami</a>
            <a href="https://yoroi-wallet.com" target="_blank" rel="noopener noreferrer">Yoroi</a>
          </div>
        </div>
      </div>
      <div className="footer-bottom">
        <p>hydra.registry &mdash; Cardano L2 Infrastructure</p>
      </div>
    </footer>
  )
}
