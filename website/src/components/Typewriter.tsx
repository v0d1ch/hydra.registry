import { useEffect, useState } from 'react'

interface TypewriterProps {
  text: string
  delay?: number
  startDelay?: number
}

export default function Typewriter({ text, delay = 50, startDelay = 900 }: TypewriterProps) {
  const [displayed, setDisplayed] = useState('')
  const [started, setStarted] = useState(false)

  useEffect(() => {
    const timeout = setTimeout(() => setStarted(true), startDelay)
    return () => clearTimeout(timeout)
  }, [startDelay])

  useEffect(() => {
    if (!started) return
    if (displayed.length >= text.length) return

    const timeout = setTimeout(() => {
      setDisplayed(text.slice(0, displayed.length + 1))
    }, delay)
    return () => clearTimeout(timeout)
  }, [started, displayed, text, delay])

  return (
    <span>
      {displayed}
      <span className="typewriter-cursor">|</span>
    </span>
  )
}
