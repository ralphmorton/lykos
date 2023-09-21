export const websocket_ = (url) => () => {
  const socket = new WebSocket(url)

  return new Promise((resolve, reject) => {
    socket.addEventListener(
      "open",
      () => resolve(socket)
    )

    socket.addEventListener(
      "error",
      (e) => reject("Failed to open websocket")
    )
  })
}

export const onError = (socket) => (onErr) => () => {
  socket.addEventListener(
    "error",
    () => onErr()
  )
}

export const onMessage = (socket) => (onMsg) => () => {
  socket.addEventListener(
    "message",
    (evt) => onMsg(evt.data)()
  )
}

export const send = (socket) => (data) => () => {
  socket.send(data)
}
