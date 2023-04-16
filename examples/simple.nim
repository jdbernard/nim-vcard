import vcard

# Reading in an existing vcard
let vcards = parseVCard3File("jack.vcf")
assert vcards.len == 1
let vcAllen = vcards[0]

assert vcAllen.email.len == 2
assert vcAllen.email[0].value == "allen@fosters.test"
assert vcAllen.n.given[0] == "Jack"

# Creating a new VCard
var vcSusan: VCard3
vcSusan.add(@[
  newVC3_N(given = @["Susan"], family = @["Foster"]),
  newVC3_Email(value = "susan@fosters.test", emailType = @["PREF",
  $etInternet]),
  newVC3_Tel(
    value = "+1 (555) 444-3889",
    telType = @[$ttHome, $ttCell, $ttVoice, $ttMsg])
])
writeFile("susan.vcf", $vcSusan)
