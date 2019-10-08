import main.Trie
import org.scalatest.FunSuite

class Tests extends FunSuite {
  test("fuzzy match") {
    val trie = Trie(1000000)

    trie.addWord("microsoft")
    trie.addWord("microsoft research")
    trie.addWord("mikrotik")
    trie.addWord("tesla")

    assert(trie.fuzzyMatch("mocrosoft", 0).isEmpty)
    assert(trie.fuzzyMatch("mocrosoft", 1).toSet == Set("microsoft"))
    assert(trie.fuzzyMatch("mocrosoft", 100).toSet == Set("microsoft", "microsoft research", "mikrotik", "tesla"))
    assert(trie.fuzzyMatch("mocrosoft", 2).toSet == Set("microsoft"))
    assert(trie.fuzzyMatchCont("microsoft", 0).toSet == Set("microsoft", "microsoft research"))
    assert(trie.fuzzyMatchCont("mocrosoft", 1).toSet == Set("microsoft", "microsoft research"))
    assert(trie.fuzzyMatchCont("microsoft ", 0).toSet == Set("microsoft research"))
    assert(trie.fuzzyMatchCont("microsoft rese", 0).toSet == Set("microsoft research"))
    assert(trie.fuzzyMatchCont("microsoft ress", 0).isEmpty)
    assert(trie.fuzzyMatch("icrosoft", 1).toSet == Set("microsoft"))
    assert(trie.fuzzyMatch("icrosoft", 0).isEmpty)
  }
}
