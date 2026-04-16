import { Routes, Route } from 'react-router-dom'
import Navbar from './components/Navbar'
import Footer from './components/Footer'
import ParticleField from './components/ParticleField'
import MouseSpotlight from './components/MouseSpotlight'
import Landing from './pages/Landing'
import Register from './pages/Register'

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
        </Routes>
      </main>
      <Footer />
    </>
  )
}

export default App
