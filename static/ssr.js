let video = null;
let capture = null;
let server = '';
let firstname = '';
let lastname = '';
let event = '';
let user = '';

window.onload = function() {
  document.getElementById('ssr').addEventListener('submit', async (e) => {
    e.preventDefault();

    server = document.getElementById('server');
    server = server ? server.value.trim() : '';
    server = server.trim();
    firstname = document.getElementById('firstname').value.trim();
    lastname = document.getElementById('lastname').value.trim();
    event = document.getElementById('event').value.trim();
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
    capture = await navigator.mediaDevices.getDisplayMedia({ video: true, audio: false });

    capture.getVideoTracks()[0].addEventListener("ended", () => {
      console.log("Screen sharing permission lost or screen sharing stopped.");
      video = null;
    });

    // Create hidden video element to render the screen stream
    video = document.createElement('video');
    video.style.display = 'none';
    document.body.appendChild(video);
    video.srcObject = capture;

    // Wait until the video has loaded enough metadata
    await new Promise(resolve => {
      video.onloadedmetadata = () => {
        video.play();
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
  if (!video || video.videoWidth === 0) {
    console.warn("Video not ready yet, skipping screenshot");
    return;
  }

  const canvas = document.createElement('canvas');
  canvas.width = video.videoWidth;
  canvas.height = video.videoHeight;
  const ctx = canvas.getContext('2d');
  ctx.drawImage(video, 0, 0, canvas.width, canvas.height);

  try {
    const blob = await new Promise(resolve => canvas.toBlob(resolve, 'image/png'));
    if (!blob) throw new Error("Failed to create blob from canvas");

    const formData = new FormData();
    formData.append('screenshot', blob, 'screenshot.png');
    formData.append('firstname', firstname);
    formData.append('lastname', lastname);
    formData.append('event', event);

    await fetch(server+'/upload', {
      method: 'POST',
      body: formData
    });

    document.getElementById('ssr').innerHTML = `<p>Logged in as ${firstname} ${lastname}.</p>`;
    console.log(`Screenshot sent for ${user} at`, new Date().toLocaleTimeString());
  } catch (err) {
    console.error("Upload failed:", err);
  }
}
