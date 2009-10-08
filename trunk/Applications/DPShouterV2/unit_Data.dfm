object Data: TData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 285
  Width = 341
  object Browser: TIdHTTP
    MaxLineAction = maSplit
    OnWorkBegin = BrowserWorkBegin
    OnWorkEnd = BrowserWorkEnd
    AllowCookies = True
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'DP-Shouter V.2 Gruesse in den Keller an Gerome'
    HTTPOptions = [hoForceEncodeParams]
    OnRedirect = BrowserRedirect
    CookieManager = Cookies
    ConnectTimeout = 3000
    Left = 16
    Top = 8
  end
  object Cookies: TIdCookieManager
    OnNewCookie = CookiesNewCookie
    Left = 16
    Top = 56
  end
end
