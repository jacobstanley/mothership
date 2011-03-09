<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <title>Mothership</title>
    <link rel="stylesheet" type="text/css" href="/reset.css">
    <link rel="stylesheet" type="text/css" href="/style.css">
  </head>
  <body>
    <header>
      <nav>
        <ul>
          <ifLoggedIn>
          <li><span class="user"><userFullName/></span></li>
          <li><a href="/">Dashboard</a></li>
          <li><a href="/logout">Logout</a></li>
          </ifLoggedIn>
          <ifGuest>
          <li><a href="/signup">Signup</a></li>
          <li><a href="/login">Login</a></li>
          </ifGuest>
        </ul>
      </nav>
      <h1><a href="/"><span class="mother">mother</span><span class="ship">ship</span></a></h1>
    </header>
    <section>
      <content/>
    </section>
  </body>
  <script type="text/javascript" src="/jquery-1.5.1.min.js"></script>
  <script type="text/javascript" src="/mothership.js"></script>
</html>
