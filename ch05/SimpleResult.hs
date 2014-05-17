import SimpleJSON

result :: JValue

result = JObject [
    ("query", JString "awkward squad haskell"),
    ("estimatedCount", JNumber 3920),
    ("moreResults", JBool True),
    ("results", JArray [
        JObject [
            ("title" JString "Simon.."),
            ("snippet", JString " fasdfas"),
            ("url", JString "http://")
        ]])
    ]
