 TestSecure
 Load←⎕SE.SALT.Load
 Load'/git/JSONServer/Source/JSONServer'
 Load'HttpCommand'
 HttpCommand.Upgrade
 js←⎕NEW JSONServer
 dyalog←2 ⎕NQ'.' 'GetEnvironment' 'Dyalog'
 js.Secure←1
 js.RootCertDir←dyalog,'/PublicCACerts/'
 js.ServerCertFile←dyalog,'/TestCertificates/Server/localhost-cert.pem'
 js.ServerKeyFile←dyalog,'/TestCertificates/Server/localhost-key.pem'
 ⎕FX'r←ValidateRequest req' '∘∘∘'
 Load'/git/JSONServer/Sample/GetSign'
 js.Start
