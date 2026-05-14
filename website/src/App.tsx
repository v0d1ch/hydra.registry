import { Routes, Route } from 'react-router-dom'
import Navbar from './components/Navbar'
import Footer from './components/Footer'
import ParticleField from './components/ParticleField'
import MouseSpotlight from './components/MouseSpotlight'
import ProtectedRoute from './components/ProtectedRoute'
import Landing from './pages/Landing'
import Register from './pages/Register'
import Explorer from './pages/Explorer'
import CreateInvoice from './pages/CreateInvoice'
import RouteExplorer from './pages/RouteExplorer'
import PaymentTracker from './pages/PaymentTracker'
import Balance from './pages/Balance'
import Docs from './pages/Docs'
import Setup from './pages/Setup'
import Dashboard from './pages/Dashboard'

function App() {
  return (
    <>
      <ParticleField />
      <MouseSpotlight />
      <Navbar />
      <main>
        <Routes>
          <Route path="/" element={<Landing />} />
          <Route path="/explorer" element={<Explorer />} />
          <Route path="/routes" element={<RouteExplorer />} />
          <Route path="/payments/:paymentId" element={<PaymentTracker />} />
          <Route path="/docs" element={<Docs />} />
          <Route path="/setup" element={<Setup />} />
          <Route path="/register" element={<ProtectedRoute><Register /></ProtectedRoute>} />
          <Route path="/invoice" element={<ProtectedRoute><CreateInvoice /></ProtectedRoute>} />
          <Route path="/balance" element={<ProtectedRoute><Balance /></ProtectedRoute>} />
          <Route path="/dashboard" element={<ProtectedRoute><Dashboard /></ProtectedRoute>} />
        </Routes>
      </main>
      <Footer />
    </>
  )
}

export default App
