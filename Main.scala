//le principe du proejt est de modeliser 
case class Queue[A](in:List[A] = Nil, out:List[A] = Nil) {//liste d'entrée in et liste de sortie out 
    
    //QUESTION 1
    
    /** Ajoute un élément `x` en tête. */
    def enqueue(x:A):Queue[A] = this match {
        case Queue(Nil, h::t) => Queue(x::(h::t).reverse, Nil)
        case Queue(i, o) => Queue(x::i, o)
    }

    /** Retire le dernier élément. */
    //Peut ne pas fonctionner
    /**
    def dequeue():(A,Queue[A]) = this match {
        //cas bloquant
        case Queue(Nil, Nil) => sys.error("vilain effet de bord, faut corriger ça")
        
        case Queue(i, Nil) => {
            val rev = i.reverse
            (rev.head, Queue(Nil, rev.tail))
        }
        case Queue(Nil, o) => (o.head, Queue(Nil, o.tail))
    }
    **/

    //Fonctionnera
    def dequeue():(Option[A],Queue[A]) = this match {
        case Queue(Nil, Nil) => (None, Queue(Nil, Nil))
        case Queue(i, Nil) => {
            val rev = i.reverse
            (Some(rev.head), Queue(Nil, rev.tail))
        }
        case Queue(Nil, o) => (Some(o.head), Queue(Nil, o.tail))
    }

    /** Accès au premier élément, s'il existe. */
    def headOption():Option[A] = this match {
        case Queue(Nil, Nil) => None
        case Queue(Nil, o) => Some(o.head)
        case Queue(i, Nil) => Some(i.reverse.head)
    }

    /** Vrai si la liste est vide. */
    def isEmpty:Boolean = in.isEmpty && out.isEmpty

    
    //QUESTION 2

    def length():Int = this match {
        case Queue(Nil, Nil) => 0
        case Queue(l, Nil) => l.length
        case Queue(Nil, l) => l.length
    }


    //QUESTION 3

    def rearOption():Option[A] = this match {
        case Queue(Nil, Nil) => None
        case Queue(Nil, o) => Some(o.reverse.head)
        case Queue(i, Nil) => Some(i.head)
    }


    //QUESTION 4

    def toList():List[A] = this match {
        case Queue(Nil, Nil) => Nil
        case Queue(i, Nil) => i.reverse
        case Queue(Nil, o) => o
    }


    //QUESTION 5

    def map[B](f:A => B):Queue[B] = this match {
        case Queue(Nil, Nil) => Queue(Nil, Nil)
        case Queue(i, Nil) => Queue(i.map(f), Nil)
        case Queue(Nil, o) => Queue(Nil, o.map(f))
    }


    //QUESTION 6

    def foldLeft[B](z:B)(f:(B, A) => B):B = this match {
        case Queue(i, Nil) => i.reverse.foldLeft(z)(f)
        case Queue(Nil, o) => o.foldLeft(z)(f)
    }

 def main(args: Array[String]):Unit = {
        
        // Test à faire

        println("== A ==")           // Initialisation : Queue vide
        var A = Queue[Int](Nil, Nil) // in = Nil, out = Nil
        println("Queue A : " + A)  
        println("\n")

        // Ajout d'un premier element
        println("== B ==")
        var B = A.enqueue(1)        // = Queue(1->Nil, Nil) : Ajout dans la pile "in"
        println("Queue B : " + B)
        println("\n")

        // Ajout d'un deuxieme élément
        println("== C ==")
        var C = B.enqueue(2)        // = Queue(2->1->Nil, Nil) : Ajout dans la pile "in"
        println("Queue C : " + C)
        println("\n")
        
        // Ajout d'un troisieme élément
        println("== D ==")
        var D = C.enqueue(3) // = Queue(3->2->1->Nil, Nil) : Ajout dans la pile "in"
        println("Queue D : " + D)
        println("\n")

        
   }
}