proc breakTest() =
  while true:
    if size == result.len: break
    let data = await socket.recv(size - result.len)
    if data == "": break # We've been disconnected.
    result.add data
