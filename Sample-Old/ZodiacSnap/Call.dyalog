 r←server Call(fn params);cmd
 :If 0=⎕NC'HttpCommand'
     ⎕SE.SALT.Load'HttpCommand'
 :EndIf
 cmd←⎕NEW HttpCommand
 fn←'/'@(=∘'.')fn
 cmd.URL←server,'/',fn
 'content-type'cmd.AddHeader'application/json'
 cmd.Command←'post'
 cmd.Params←params
 r←cmd.Run
