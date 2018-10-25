 TestSecure pathToJSONServer
 :If 0=⎕NC'JSONServer'
     ⎕SE.SALT.Load pathToJSONServer,'/Source/JSONServer.dyalog'
 :EndIf
 ⎕SE.SALT.Load pathToJSONServer,'/Sample/GetSign.dyalog'
 js←⎕NEW JSONServer
 dyalog←2 ⎕NQ'.' 'GetEnvironment' 'Dyalog'
 js.Secure←1
 js.SSLValidation←64 ⍝ request, but do not require a certificate
 js.RootCertDir←dyalog,'\TestCertificates\Ca\'
 js.ServerCertFile←dyalog,'\TestCertificates\Server\localhost-cert.pem'
 js.ServerKeyFile←dyalog,'\TestCertificates\Server\localhost-key.pem'
 ⎕FX↑'r←ValidateRequest req' 'r←0' ':if ~0∊⍴req.PeerCert ⋄ ''Subject'' ''Valid From'' ''Valid To'',⍪req.PeerCert.Formatted.(Subject ValidFrom ValidTo) ⋄ :EndIf '
 js.Start
