//Queue first in first out 

//classe Queue ,une structure de données, class immuable
//le principe du projet est de modeliser avec des liste (deux listes)
//données:  list in ,list out
case class Queue[T](in:List[T]=Nil ,out:List[T]=Nil) {//liste d'entrée in  et liste de sortie out 

//out pour enlever un element 
//in pour inserer un element
//mettre un element dans la queue 
/** Ajout d'un élément `x` en tête. cette fonction nous permet */
def enqueue(X:T):Queue[T] = Queue(X::this.in,this.out)

//enlever un element de la queue(de out)
/** Retire le dernier élement */
def dequeue():(Option[T],Queue[T]) = out match {
    
    case x::y => (Option(x), Queue(in, y))
    case Nil => in.reverse match {
    case x :: y => (Option(x), Queue(Nil, y))
    case Nil => (None, Queue(Nil, Nil))
      
    }
  }
//obtenir le dernier element inséré dans in 

/** Accès au premier élément, s'il existe (dernier élément entré). */
def headOption():Option[T] =  // regarder la tete de la liste 
 // verifier si les listes sont vides
  if( this.isEmpty)  None  //  impossible de renvoyer la tete 
  
  else if((!this.in.isEmpty) && (this.out.isEmpty)) Some(this.in.head)//sois je renvoie  

  else Some(this.out.reverse.head) // somme de la tete 

//il suffit de  regarder les deux liste in et out 
/** Vrai si la liste est vide faux sinon. */
def isEmpty:Boolean = in.isEmpty && out.isEmpty
//nbr d'elements a l'inter la queue  
/** retourner la taille de la queue le nombre d'lement a l'interieur de la queue  */
def length():Int = this.in.length+this.out.length 


/** obtenir l'élément de queue si la liste n'est pas vide  */
def rearOption():Option[T] = this match {
      
  case Queue((Nil),(Nil)) => None
  case Queue((x),(Nil))   => Some(x.last)
  case Queue((Nil),(x))   => Some(x.head)
  case Queue((x),(y))     => Some(x.last)
  
  }
// transformer la queue en liste tous les elemnt qui sont a linterieur de in et out 
/** convertit la Queue en liste simplement chaînée */
def toList():List[T] =

  if (this.isEmpty) Nil

  else if((!this.in.isEmpty) && (this.out.isEmpty)) this.in

  else this.out.reverse ++this.in



/** Effectue la fonction map sur les deux listes */
def map[B](f:T => B):Queue[B] = Queue(in.map(f), out.map(f))

/** Effectue la fonction foldLeft sur les deux liste (queue) */
def foldLeft[B](start:B)(f:(B, T) => B):B = toList().foldLeft(start)(f)

/** Question 8   Ré-implantez isEmpty avec match. return vrai si vide  */
def isEmptyMatch():Boolean = this match{
  
  case Queue(Nil, Nil) => true
  case Queue((x::_),(Nil)) => false
  case Queue((Nil),(x::_)) => false
  case Queue((x),(y)) => false
  
  

  }
}


object TestQueue {
  

  def main(args: Array[String]): Unit = {
       // Initialiser Queue, queur est vide 
        println("********************A***********************")  
        var A = Queue[Int](Nil, Nil) // in = Nil et  out = Nil
        //la queue A est vide 
        println("Queue A : " + A)  
        println("\n")

        // Ajout d'un premier element
        println("******************* B ****************")
        var B = A.enqueue(1)        // = Queue(1->Nil, Nil) : Ajout 1 dans la liste "in"
        println("Queue B : " + B)
        println("\n")

        // Ajout d'un deuxieme élément
        println("****************** C ***************")
        var C = B.enqueue(2)        // = Queue(2->1->Nil, Nil) : Ajout 2 dans la liste "in"
        println("Queue C : " + C)
        println("\n")
        
        // Ajout d'un troisieme élément
        println("*******************D*****************")
        var D = C.enqueue(3) // = Queue(3->2->1->Nil, Nil) : Ajout dans  la liste "in"
        println("Queue D : " + D)
        println("\n")

        /* Suppression d'un element :
            on inverse la pile "in", on place le tail dans la liste "out"
            et renvoyer le head comme élément supprimé, 
            c'est à dire le premier élément que l'on a inséré : 1
            puis on va vider "in"
        */
        println("******************E ***************")
        // = (1, Queue(Nil, 2->3->Nil) ici 'in' est inversée et placée en `out`
        val (a, e) = D.dequeue()
        println("supprime : " + a)
        println("Queue : " + e)
        println("\n")
        
        /* Ajout d'un nouveau élément
            On insere  à nouveau dans la liste "in"
            Nous aurons donc, in = Cons(4, Nil), out = Cons(2, 3)
        */
        println("*************** F *****************")
        var F = e.enqueue(4) // = Queue(4->Nil, 2->3->Nil)
        println("Queue F : " + F)
        println("\n")
        
        println("******************** G ********************")
        var G = F.enqueue(5) // = Queue(5->4->Nil, 2->3->Nil)
        println("Queue G : " + G)
        println("\n")

        println("******************H ************")
        var (b, h) = G.dequeue() // = (2, Queue(5->4->Nil, 3->Nil)
        println("supprime : "+ b)
        println("Queue h : " + h)
        println("\n")

        println("***************** I *************")
        var (c, i) = h.dequeue() // = (3, Queue(5->4->Nil, Nil))
        println("supprime : "+ c)
        println("Queue i : " + i)
        println("\n")

        println("********************* J *****************")
        var (d, j) = i.dequeue() // = (4, Queue(Nil, 5->Nil))
        println("supprime : "+ d)
        println("Queue j : " + j)
        println("\n")

        println("***************** K ***************")
        var (ee, k) = j.dequeue() // = (5, Queue(Nil, Nil))
        println("supprime : "+ ee)
        println("Queue k : " + k)
        println("Queue empty  : " + k.isEmpty) // = True
        println("\n")

        // 2. Implantez une méthode length qui retourne la taille de le queue.
        println("***************** Length ************")
        println("Queue C : " + C)
        println("Taille : " + C.length)
        println("\n")
        println("Queue G: " + G)
        println("Taille : " + G.length)
        println("\n")

        /* 3. Implantez une méthode rearOption 
            qui retourne le dernier élément de la queue 
            (celui inséré en premier) sans la modifier */
        println("********** RearOption ***********")
        println("Queue C : " + C)
        println("RearOption : " + C.rearOption)
        println("\n")
        println("Queue G : " + G)
        println("RearOption : " + G.rearOption)
        println("\n")

        /*
        4. Implantez une méthode toList qui convertit 
        la Queue en liste simplement chaînée.
        */
        println("**************** ToList **************")
        println("D : " + D) 
        println("D to List : " + D.toList) 
        println("\n")
        
        println("G : " + G) 
        println("G to List : " + G.toList) 
        println("\n")

        /* 5. Implantez une méthode map sur Queue.*/
        println("************* Map ***************")
        println("G : " + G) 
        println("G map : " + G.map(x => x*3)) 
        println("\n")

         /* 6. Implantez une méthode foldLeft sur Queue.*/
        println("****************** FoldLeft *************")
        println("Queue G : " + G) 
        println("G foldleft : " + G.foldLeft(1)(_ * _)) 
        println("\n")
        
        println("Queue A : " + A) 
        println("A foldleft : " + A.foldLeft(1)(_ * _)) 
        println("\n")
        

        // 8. Ré-implantez isEmpty avec match.
        println("Queue A : " + A)
        println(A.isEmptyMatch)
        println("Queue G : " + G)
        println(G.isEmptyMatch)
  }
}