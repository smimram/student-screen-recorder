let videoEl = null;
let captureStream = null;
let firstname = '';
let lastname = '';
let user = '';

window.onload = function() {
  document.getElementById('ssr').addEventListener('submit', async (e) => {
    e.preventDefault();

    firstname = document.getElementById('firstname').value.trim();
    lastname = document.getElementById('lastname').value.trim();
    user = `${firstname} ${lastname}`;
    if (!firstname || !lastname) {
      alert("Please enter your first and last name.");
      return;
    }

    await startCapture();
  });
};

async function startCapture() {
  try {
    captureStream = await navigator.mediaDevices.getDisplayMedia({ video: true, audio: false });

    // Create hidden video element to render the screen stream
    videoEl = document.createElement('video');
    videoEl.style.display = 'none';
    document.body.appendChild(videoEl);
    videoEl.srcObject = captureStream;

    // Wait until the video has loaded enough metadata
    await new Promise(resolve => {
      videoEl.onloadedmetadata = () => {
        videoEl.play();
        resolve();
      };
    });

    console.log(`Screen capture started for: ${user}`);
    takeScreenshot();
    setInterval(takeScreenshot, 15 * 1000);
  } catch (err) {
    console.error("Error starting capture:", err);
  }
}

async function takeScreenshot() {
  if (!videoEl || videoEl.videoWidth === 0) {
    console.warn("Video not ready yet, skipping screenshot");
    return;
  }

  const canvas = document.createElement('canvas');
  canvas.width = videoEl.videoWidth;
  canvas.height = videoEl.videoHeight;
  const ctx = canvas.getContext('2d');
  ctx.drawImage(videoEl, 0, 0, canvas.width, canvas.height);

  try {
    const blob = await new Promise(resolve => canvas.toBlob(resolve, 'image/png'));
    if (!blob) throw new Error("Failed to create blob from canvas");

    const formData = new FormData();
    formData.append('screenshot', blob, 'screenshot.png');
    formData.append('firstname', firstname);
    formData.append('lastname', lastname);

    await fetch('/upload', {
      method: 'POST',
      body: formData
    });

    document.getElementById('ssr').style.display = 'none';
    console.log(`Screenshot sent for ${user} at`, new Date().toLocaleTimeString());
  } catch (err) {
    console.error("Upload failed:", err);
  }
}
