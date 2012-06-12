<h2>Student &rarr; Login</h2>
  
<dfForm id="login-form">

  <dfChildErrorList />

  <table>
    <tbody>
      <tr>
        <td>
          <dfLabel ref="username">Matrikelnummer</dfLabel>
        </td>
        <td>
          <dfInputText ref="username" placeholder="12345" />
        </td>
      </tr>
      <tr>
        <td>
          <dfLabel ref="password">Passwort</dfLabel>
        </td>
        <td>
          <dfInputPassword ref="password" />
        </td>
      </tr>
      <tr>
        <td>
          <dfLabel ref="remember">Eingeloggt bleiben</dfLabel>
        </td>
        <td>
          <dfInputCheckbox ref="remember" />
        </td>
      </tr>
    </tbody>
  </table>

  <dfInputSubmit value="Login" />

</dfForm>

<a href="/register">neuen Account anlegen</a>
