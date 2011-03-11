<apply template='default'>

<h2>All Repositories</h2>

<ul id='repositories'>
  <repositories>
    <li>
      <h3><a href="/$(name)"><name/></a></h3>
      <p class='description'><description/></p>
    </li>
  </repositories>
</ul>

<ifLoggedIn>
  <a class='button' href='/repositories/new'>New repository</a>
</ifLoggedIn>

</apply>
