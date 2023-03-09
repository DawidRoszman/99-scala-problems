@main
def Learning2(): Unit =
  val l = List(1, 1, 1, 2, 3, 4, 1, 5, 5, 6, 7, 8, 9, 10)
  println(pack(l))

// P04 Find the number of elements of a list.
//     Example:
//     scala> length(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 6

def len(list: List[Int]): Int = {
  @annotation.tailrec
  def go[A](list: List[Int], aku: Int): Int = {
    list match {
      case Nil => aku
      case _ => go(list.tail, aku + 1)
    }
  }
  go(list, 0)
}

// P07 (**) Flatten a nested list structure.
//     Example:
//     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//     res0: List[Any] = List(1, 1, 2, 3, 5, 8)
// flatMap is a higher-order function in Scala that operates on collections such as lists, sets, and sequences. It is a combination of two other functions: map and flatten.

// The map function applies a given function to each element of a collection and returns a new collection with the results. The flatten function, on the other hand, flattens nested collections into a single collection.

// The flatMap function applies a given function to each element of a collection and then flattens the results into a single collection. It is useful when we have a nested collection and we want to apply a transformation to each element in the nested collection and then flatten the results into a single collection.

type x = "A" | "B" => 0 | 1
def flatten(list: List[Any]): List[Any] = {
  list flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}

// P08 (**) Eliminate consecutive duplicates of list elements.
//     If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

def eliminateDuplicates[A](list: List[A]): List[A] = {
  @annotation.tailrec
  def go[A](list: List[A], aku: List[A]): List[A] = {
    list match {
      case head :: next :: tail =>
        if head == next then go(next :: tail, aku) else go(next :: tail, aku :+ head)
      case _ => aku
    }
  }
  go(list, Nil)
}

// P09 (**) Pack consecutive duplicates of list elements into sublists.
//     If a list contains repeated elements they should be placed in separate sublists.
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

def pack(list: List[Any]): List[Any] = {
  @annotation.tailrec
  def go(list: List[Any], aku: List[Any]): List[Any] = {
    list match {
      case Nil => aku
      case head :: tail =>
        go(tail.dropWhile(_ == head), aku ::: List(list.takeWhile(_ == head))) // :D
    }
  }
  go(list, Nil)
// takeWhile dropWhile
}

// P10 (*) Run-length encoding of a list.
//     Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
// ☭wujcio stalin☭

// ǝlɐɔS ɯɐɥɔoʞ
// ɐʍouoƃo ǝɾɔuǝɹnʞǝɹ ı ǝlɐɔS ɯɐɥɔoʞ
// P11 (*) Modified run-length encoding.
//     Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
//     Example:
//     scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

// P12 (**) Decode a run-length encoded list.
//     Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
//     Example:
//     scala> decode(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

// P13 (**) Run-length encoding of a list (direct solution).
//     Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
//     Example:
//     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWNXXXXNNNXXXXXXXXXXXXXXXNMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWWXNMMMMMMWWWWXkdoc::ccccccllc:;;;;;;,,cOWWWWMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKo:,:0WMMWKxodkO000Oo;...:x0KK0Okkkkkkdc:colcoKWMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKxool:,;cdkkxkOxddxOKNWWWXxc;,;loddOXNNNWNNX0kxdlc;lxxdxXMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOc'',cox0XXkoodk0XNNWWWWWWNXK0kdc;'.,lkKXXXXXXXXXK0kdc:,,ckXWMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWNXXNNWMMMMMMMMMMMMMMMMMWNOl,;okKXKXklcoOXNNNWWWWNNNXKOOO000OOkdc,,lOKXXXXXXXXXKK0Okdc;:kNWMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOlccco0WMMMMMMMMMMMMMMMW0dlcoOXXkl::;,,;cxKNNNNNNXX0xc;::ccc:lk0Odccok0XXXXXXXXXXXXKK0ko::xNMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOdodkNMMMMMMXkdxkkxdollxNMMMMMMMMMMMMMWX0KXN0doxOOOOOOOOkddk0XXKOoldkkOOOOkkkdlok0OOOKXXXXXXXXXXXXXXKK0klccdXMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXOxdddlcoxKWMMMO::kNNK0x:.,0MMMMMMMMMMMMMMMMN0xod0NMMWWMWMMN0xddxxdookXWMMMMMMMWKxoldOKXXXXXXXXXXXXXXXXK00K0kl:lxKWMMMMMMM
// MMMMMMMMMMMMMMMMMNK0KKKKNWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWx;l0XX0dc,:kXWWO;:kXNK0xl;;dKNMMMMMMMMMMMMNKkoodk0NMMMMWWWWMWNOl;;d0NWKk0NMMMWWMMWXxclxO0KKXXXXXXXXXXXXK0000K0kc':kXWMMMMM
// MMMMMMMMMMWNNNNNXxc:::::o0NWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWx;l0NNKOko:':0WKocxKXXK0kdc,,xWMMMMMMMMMMNd;o00l.,xXNWMMMMMMMNkccxXWNO:.,xXNWWMMMWWNKxccd0KXXXXXXXXXXXXK00000K0kd:':0MMMMM
// MMMMMMMMWKdlllccodkkxxkko:ckWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWx;lONNXK0kl''xWWN0olxKXXKOo:,dNMMMMMMMMMMNd,'',oxl,,dNMMMWWMMNk:lKWO:'cdl,,dNMMMWWMMNOc:oOKKKKKXXXXXXXXKK0000000Oo;;kWMMMM
// MMMMMMWOxxxkxo:;cxKXKK00OxlclxXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOxod0XXKOxl::o0WNo'cOXXX0kdlclxNMMMMMMMMNd;'..,:,.'oNMMMWMMMNkcl0NO;.':,.'dNMMMWWMMNkc:ok0000KKXXXXXXXXXK00OO000kdcclOWMM
// MMMMMMXc;xXNX0o::dKXXKOkO0kc.'OMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMKc;dKXXK0Oo;.cXWKdcldxOKK0x:.,OMMMMMMMMNxccc:'..,ckNMMMWMMMNk:l0WKd;...;o0WMMMWWMMNOlcokO0000KKXXXXXXXXXK0OO0000Od;.:XMM
// MMMMMMNxlokKXKkocok0KKOOOOko;;dKNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNxlox0KXK0xc,;xXWWk,.'o0XKko:;dKWMMMMWNKkdoccc:cox0WMMWWWMMNkc:xKKkoc:clxKWMMMMWWNKkoodkOO00KKKXXXXXXXXXK000000KKkc,:kKN
// MMMMMMMWNOllx0K0kocoxO0K0OkOOo;,dWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWNd:lkKXKOkdc,:kXx...lOXK0Oxl,'dNMMM0llxOkl;;:o0XNNNWWWWWWXOl;;ckKKxod0NWMMMMWNKxooxxkkkkkOOO00KKXXXXXXXKK0000KKKOd:',k
// MMMMMMMMMWXklcx0K0xocok0KOkOOd;.lNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWx,:xKXXK0kxoc:;'..'o0XXK0Od;.lXMWWk;:x0kl,,;:lollxKWWWXkolodddoodOXNWWWWWWNKxoodxkxxxdolc:::coxOKXXXXXXK0000KKXKOo''x
// MWKxddONMMMWKkolx00x::d0K0OOOkdlclxXMMMMMMMMMMMMMWKxdddddddxKWMMMMMWKxddddddddddddo:,;cokKKK0OkkdlcccokKXXK0kxdooddddc,;:ldkkkkxxxxdooodkkdodkOOOxoldxxxxxxxkkdodxkkkkxxxoc,....';cdOKXXXXK00OO0KXX0o,.x
// OkoclccokXMMMMk,;d0OdloxOKK0kO0Ol.'kMMMMMMMMMMMWKkdoooooooollx0WMWNOdooooooooooooooddolox0XXK00OOOkOOO00KK0kxkO0koccloollokXNNXXKKK0kl;',cxOOOOOOOkxoc::cccccodxkkxkkkkkkxl;.';cccloxkO0KXK0OkO0KXXOo,'x
// ,cx0KOd:,cOXWMKo:cxO0kl,;oOOkkOxo;,dXWMMMMMMMWXkllkXXXXXKKKOd:cOKxcckXNXXXXXXXXXXXXXXXK00KKKK00OkkkkkkOOOOkkxkkOOOOOO0000KKKK0OkkxxxkOxc',lk0OOOOOkkkxxxxxxxxxxkkkkkkkkkxo:'':xKKK00Okxk0XK0kkO0KXXOo,'x
// ,cx0KXKOxc;:kNWNOc;oO0o,.,lk00kxkkd:,lKWWWWWNxcoOKK0OOOOO00OOxl:,.'cx00OOkkkkkkkkOOOOOOOOOOkkxxxkkkxxxxxxxxkkkkOOOOOO000OkkkkxxxkkkO00Ol''lO0OOOOOOOkkkkkkkkkkkkkkkkxdoc:,...cOXXXKKKK0KKK0OkkO0KXXOo,'x
// 0OolkKXXKOdc::cl;..:dkOxolok00OxkOx:..:oolllodk0K0OkxkOOO00Oxc;,,ck0xc:c:::::::::::::ldkOOkkkkkkkkxxxxxkkkkkOOOOOkkkkO0kl;,,,,;lk000KKOl',oO000000OOOOOOOOkkkkkkkOOkxo:'.....ckKXXXXXXXXK00OkkO0KXXOo,'x
// MWKxookKXXKOdc::::cldkOOkkxxkOOkxolodxxxxdoodkKKKOdc::cccllccoddOXMMXkxxxxxkkxxxxxxxxdlox0KK0OOOkkxxxxkkOOOOOOOOOOOOOxocloddl;'':clcccccclxO000000000OOOOOOOOOOOOkxo:,..,c:'.:kKXXXXXKKK00OOkkO0KXXOo,'x
// MMMWKdoxkkxxO0OOkkkkOOOkxxxxkkkxold0XNNXK0OOOOOOxoccccccccclOWMMMMMMMMMMMMMMMMMMMMMMWKc;dKNXK0OOOkkxxxkOOOOOOOOkkO00Ol''oKNXOl'.',;;;;:ok000000000000000OOkxdooolcc::,..cxx:':kXXXXXKK00000OkkO0KXXOo,.x
// MMMMMWXxc;;;lk0K00OOOOOOkxkOOxl:oOKXXK0OkkkOOOkl::xNWWWWWWWMMMMMMMMMMMMMMMMMMMMMMMMMMNdcoOKKXK00OOkxxxkkkOOOOOOOkO0KOo,'lO0xl;,,:clodddddddxxxxxxxdddxxdddol::;;;;cx0x;.:xx:':kXXXXKKK0000OOkkO0KKKOo,.x
// MMMMMMMWXKKOc,lk0K0OOOkkxkO00xc;o0XK0kkxxkOOkl,:kXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWXd,ckXNXK00OOkkkxkkkOOOkOOO0KOl,,lkOdodxdc;,,,,,,,,,,,,,,,,,,,,,,,,;lx00000KNXk;.;dd;':kKXXKKK000OOOOkkO0KK0kl''x
// MMMMMMMMMMMWXkl;lx0K0OkkkO0K0xc:oOK0kxkOOOxl;lkKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMk,;kXNXK00000OOkkxkkOOkO00Od:cxO0OOkO0XX0kkxc'.'coolcccclloddddxxxk0XWWWMMMWNk;.:xx:':kKXK000OOOOOOOkkO0K0xllxOX
// MMMMMMMMMMMMMMk,,d0XK000OO000OxoccldxkkOKOo'.xMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOxdxOKKKKK000OOkxxxkkkO00x:.:XNklodk0KXNNWNXkocldOKXXXXXNNWNNNWWWWWWWWWMWNKd:''ckxc,:kKK000OOOOOOOOOOO00xc'cXMM
// MMMMMMMMMMMMMMXxlox0KKK0OOOO00KOdc:okOO0K0d;'xWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOoldOKKKKK00OOkkkkOOOOkocckNWkc;:ok0KXXNNNNKo''dXWMMMMMMMMMMMMMMMMWMMWKxc'..;d0Ol;:x0K0OOOkkkkOkkO000ko:ckWMM
// MMMMMMMMMMMMMMMWNxcox00K000OOOO00OOOOkkOOkdll0MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMW0oldOKKKKKK00OOOO0Oo;,xWWMMMNk;:oO00KKXNNXx;,:xO0KKKKXXXXXXXXXXKKK0x:'.''';o0Ol;cx00OOkkxxxkOOO0K0x:;xNWMMM
// MMMMMMMMMMMMMMMMMWXkccx0KK0OkkkOOOO0OxlccclONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWN0ocoOKKKKK00OO00Ol''kMMMMMMO;':okO00KXXNX0xc,',;;;;;;;;;;;;;;;;;,'''';;,:dK0l,:dOOOkkxxxxkOOO00kl',OMMMMM
// MMMMMMMMMMMMMMMMMMMWKkdllloxO00OkxkkOkddddookXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXOdlclloxOK0O00Ol''kWMMMMMN0d;,cdO000KXNNX0d:'...................,;;;:lkXN0l,:dOOkxxxxkkO0OOxl;:d0NMMMMM
// MMMMMMMMMMMMMMMMMMMMMWKxddddddoloxOOOOkOOOkdloxxxONMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOdddddoxkO00Oxl:lxKWMMMMMW0o:;cdO00KKXXNNKxc,..........''''''',;coOXNXkoccoxkxxxkkO000kdc;:o0WMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMWKdc::coxddxkOOO00koc:ccd0XWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMW0:;oO00O00x;.cXMMMMMMMMKo;:cdk000KKXNNKkc,..';:::clllllc::lONWN0d::ldxkxxkO0KK0Oxc;;oKWMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMWNNNOl;;;cxOOkOOOOO00kocl0NNNWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXo:okOOO00kc':0WMMMMMMMMWXd,,cdk000KXXNNKxc;:ok0KXXXXXXXKKXNN0d:coxxkkkOO0KKKOxl;,dXWMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWXK00d:,,;lk0K00OOOkdllccdXWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNKdcok00OOkxl;c0MMMMMMMMMWKx;,cdk000KKKKXKkl;:d0XXXXXXXNWN0o:coxkOO0000000Oxc,;dKNMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNOxxxoc:::lxO00kdoodddoookNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWx,:x00OO0Oo'.xMMMMMMMMMMMW0dc,:codkOO0KKKOdc;;::ccccloooodxkO00KKK0Okxlc:,:d0WMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMXxlll::oxOOOkkOkdollldkkk0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMKxoodkOO0Oxl:lxKWMMMMMMMMMMMKdlooc:codxkKXNKxllodddoollok0KKKK0Okdoc:cooldKWMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKl;cdkOOkxkkO0Okdc::lkKK0XWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKc;oO00000x:.:XMMMMMMMMMMMMMMMW0c:clcloxOOOOOOOOOkkxxxxxxxxdolclc:cOWMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKl,cx00OkkkkkkOO00kdc;,;OWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMKl;dOKKK00xc'cKMMMMMMMMMMMMMMMMMWNNNx;',,,,,,,,,,,,,,,,,,,',',xXNNWMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKxOKNNNXKKKKXXNNNXKOkkkKMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN0OKXNNNNXKOk0NMMMMMMMMMMMMMMMMMMMMMXkxxxxxxxxxxxxxxxxxxxxxxxkXMMMMMMMMMMMMMMMMMMMMMMMMMMMM
