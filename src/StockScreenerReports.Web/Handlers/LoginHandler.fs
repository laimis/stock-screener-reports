module StockScreenerReports.Web.Handlers.LoginHandler

open System.Net.Http
open System.Security.Claims
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Http
open StockScreenerReports.Web.Shared.Views


let client = new HttpClient()

[<CLIMutable>]
type LoginModel = { Username : string; Password : string; ReturnUrl : string option }
    
let loginHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let content =
            div [ _class "container" ] [
                form [ _class "box"; _action "/login"; _method "post" ] [
                    h1 [ _class "title is-3" ] [ str "Please sign in" ]
                    div [ _class "field" ] [
                        label [ _class "label"; _for "inputUsername" ] [ str "Username" ]
                        div [ _class "control" ] [
                            input [ _type "text"; _id "inputUsername"; _name "Username"; _class "input"; _placeholder "Username"; _required; _autofocus ]
                        ]
                    ]
                    div [ _class "field" ] [
                        label [ _class "label"; _for "inputPassword" ] [ str "Password" ]
                        div [ _class "control" ] [
                            input [ _type "password"; _id "inputPassword"; _name "Password"; _class "input"; _placeholder "Password"; _required ]
                        ]
                    ]
                    input [ _type "hidden"; _name "ReturnUrl"; _value (ctx.Request.Query["returnUrl"].ToString()) ]
                    div [ _class "field" ] [
                        div [ _class "control" ] [
                            button [ _type "submit"; _class "button is-primary" ] [ str "Sign in" ]
                        ]
                    ]
                ]
            ]

        ([content] |> mainLayout "Login") next ctx
        
let verifyLoginHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindFormAsync<LoginModel>()
            
            let request = new HttpRequestMessage(HttpMethod.Post, "https://app.nightingaletrading.com/api/account/login")
            request.Content <- new StringContent( $"""{{ "email": "%s{model.Username}", "password": "%s{model.Password}" }}""", System.Text.Encoding.UTF8, "application/json")
            
            let! response = client.SendAsync(request) |> Async.AwaitTask
            
            // Validate the credentials and authenticate the user
            if response.IsSuccessStatusCode then
                let claims = [ Claim(ClaimTypes.Name, model.Username) ]
                let identity = ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme)
                let principal = ClaimsPrincipal(identity)
                do! ctx.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, principal)
                let returnUrl = model.ReturnUrl |> Option.defaultValue "/"
                return! redirectTo false returnUrl next ctx
            else
                ctx.SetStatusCode 401
                return! text "Invalid credentials" next ctx
        }

let logoutHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            do! ctx.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme)
            return! redirectTo false "/login" next ctx
        }