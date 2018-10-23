 TestSecure pathToJSONServer
 ⎕SE.SALT.Load'HttpCommand'
 {}{0::⍬ ⋄ HttpCommand.Upgrade}⍬
 :If 0=⎕NC'JSONServer'
     ⎕SE.SALT.Load pathToJSONServer,'/Source/JSONServer.dyalog'
 :EndIf
 ⎕SE.SALT.Load pathToJSONServer,'/Sample/GetSign.dyalog'
 js←⎕NEW JSONServer
 dyalog←2 ⎕NQ'.' 'GetEnvironment' 'Dyalog'
 js.Secure←1
 js.RootCertDir←dyalog,'\TestCertificates\Ca\'
 js.ServerCertFile←dyalog,'\TestCertificates\Server\localhost-cert.pem'
 js.ServerKeyFile←dyalog,'\TestCertificates\Server\localhost-key.pem'
 js.Start
