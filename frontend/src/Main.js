export const pickFile_ = async () => {
  const result = await window.showOpenFilePicker()
  const file = await result[0].getFile()

  return new Promise((resolve) => {
    const reader = new FileReader()

    reader.onloadend = (evt) => {
      resolve(evt.target.result.replace('data:text/plain;base64,', ''))
    }

    reader.readAsDataURL(file)
  })
}
