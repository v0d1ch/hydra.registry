import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { BrowserRouter } from 'react-router-dom'
import { NetworkProvider } from './context/NetworkContext'
import { WalletProvider } from './context/WalletContext'
import { UserProvider } from './context/UserContext'
import App from './App'
import './styles/global.css'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <BrowserRouter>
      <NetworkProvider>
        <WalletProvider>
          <UserProvider>
            <App />
          </UserProvider>
        </WalletProvider>
      </NetworkProvider>
    </BrowserRouter>
  </StrictMode>,
)
