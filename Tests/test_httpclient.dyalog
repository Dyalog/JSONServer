 {r}←test_httpclient dummy;result;t
 t←#.test_httpcommand
 result←#.HttpCommand.Get t._httpbin,'/gzip'
 r←(0 200,t._true,(⊂'gzip'))t.check result.(rc HttpStatus),((t.fromJSON result.Data).gzipped),⊂result.Headers #.HttpCommand.Lookup'content-encoding'
