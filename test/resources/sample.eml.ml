let render_home tasks =
  <html>
  <body>
%   tasks |> List.iter begin fun (name, complete) ->
      <p>Task <%s name %>:
%       if complete then begin
          complete!
%       end
%       else begin
          not complete.
%       end;
      </p>
%   end;
  </body>
  </html>

let render_form request =
  %% response
  <form method="POST" action="/">
    <%s! Dream.csrf_tag request %>
    <input name="message" autofocus>
  </form>
  %%

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [Dream.get "/" (fun _ -> Dream.html (render_home []))]
