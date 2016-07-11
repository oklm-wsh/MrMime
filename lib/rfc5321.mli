open Parser

type err += Invalid_ipv4
type err += Invalid_ipv6
type err += Invalid_address_literal of string
type err += Unknown_tag of string

type literal_domain = ..
type literal_domain += IPv4 of Ipaddr.V4.t
type literal_domain += IPv6 of Ipaddr.V6.t

val is_dcontent : char -> bool

val ipv4_address_literal    : Ipaddr.V4.t t
val ipv6_addr               : Ipaddr.V6.t t
val ipv6_address_literal    : Ipaddr.V6.t t
val let_dig                 : char t
val ldh_str                 : string t
val general_address_literal : literal_domain t
val address_literal         : literal_domain t
