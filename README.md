Student screen recorder
=======================

This is a small web application to regularly take a screenshot of the screen of students, typically to make sure that there is no cheating during an exam. Image capture is done by using capabilities of modern browsers.

## Running the server

In order to start, you should begin by running the server by typing

```
make serve
```

in the main directory. This runs the server which is used to collect screenshots and view them. You can test that it is running by going to <http://localhost:8080>.

## Preparing an exam

The screenshot are collected per _event_ (an event is typically an exam) and stored in `data/event/`. In order to create a new event, simply create a subdirectory there; for instance, named `my-exam`. The in the webpage for your exam, you should add the following header

```html
<script src="https://localhost:8080/ssr.js"></script>
```

Of course, you should replace `localhost` by the name of the server. You should also add the following html code

```html
<form id="ssr">
<label for="firstname">First name:</label>
<input type="text" id="firstname" required><br>
<label for="lastname">Last name:</label>
<input type="text" id="lastname" required><br>
<input type="hidden" id="server" value="https://localhost:8080">
<input type="hidden" id="event" value="my-exam">
<button type="submit">Login and upload screenshots</button>
</form>
```

which will add a form allowing students to login with their name (no verification is performed here). Again, you should replace `localhost` with the actual host above and the value of `event` by the actual event instead of `my-exam`. And that's it! When student log in, they will be asked permission to share their screen, which they should give.

In order to test this without making an exam, you can go at <https://localhost:8080/admin/test/> (you need to be logged in, see below).

## Monitoring

All the collecting of data and monitoring is done at the url <https://localhost:8080/admin/> where you can see the screenshots of the students currently uploading and retrieve the videos for past events (which requires `ffmpeg` to be installed).

The administrative pages are protected by a login/password which is `admin`/`admin` by default. In order to change the password, create a file `data/ssr.yml` with content of the form

```yaml
admin_password: "secretpass"
```

(more options for the program should go there too).

## Deploying

In order to deploy on a server, you can create a [docker image](docker/Dockerfile).
