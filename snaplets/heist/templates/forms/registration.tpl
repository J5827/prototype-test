<h2>Account anlegen</h2>
  
<dfForm id="registration-form">

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
          <dfLabel ref="firstname">Vorname</dfLabel>
        </td>
        <td>
          <dfInputText ref="firstname" />
        </td>
      </tr>
      <tr>
        <td>
          <dfLabel ref="lastname">Nachname</dfLabel>
        </td>
        <td>
          <dfInputText ref="lastname" />
        </td>
      </tr>
      <tr>
        <td>
          <dfLabel ref="email">Email</dfLabel>
        </td>
        <td>
          <dfInputText ref="email" />
        </td>
      </tr>
    </tbody>
  </table>

  <dfInputSubmit value="Absenden" />

</dfForm>
