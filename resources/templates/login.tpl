<apply template='default'>

<h2>Log in to Mothership</h2>

<ifLoginFailure>
<p class="error">Invalid username or password.</p>
</ifLoginFailure>

<form method='post'>
  <dl>
    <dt><label for='username'>Username</label></dt>
    <dd><input id='username' name='username' type='text' class='autofocus'></dd>
  </dl>

  <dl>
    <dt><label for='password'>Password</label></dt>
    <dd><input id='password' name='password' type='password'></dd>
  </dl>

  <button type='submit'>Log in</button>
</form>

</apply>
