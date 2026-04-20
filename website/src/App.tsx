import { Routes, Route } from 'react-router-dom'
import Navbar from './components/Navbar'
import Footer from './components/Footer'
import ParticleField from './components/ParticleField'
import MouseSpotlight from './components/MouseSpotlight'
import Landing from './pages/Landing'
import Register from './pages/Register'
import Explorer from './pages/Explorer'
import CreateInvoice from './pages/CreateInvoice'
import RouteExplorer from './pages/RouteExplorer'
import PaymentTracker from './pages/PaymentTracker'

function App() {
  return (
    <>
      <ParticleField />
      <MouseSpotlight />
      <Navbar />
      <main>
        <Routes>
          <Route path="/" element={<Landing />} />
          <Route path="/register" element={<Register />} />
          <Route path="/explorer" element={<Explorer />} />
          <Route path="/invoice" element={<CreateInvoice />} />
          <Route path="/routes" element={<RouteExplorer />} />
          <Route path="/payments/:paymentId" element={<PaymentTracker />} />
        </Routes>
      </main>
      <Footer />
    </>
  )
}

export default App
