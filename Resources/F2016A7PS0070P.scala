package pplAssignment

object f2016070{
def dotProduct(matrix_1:List[List[Double]],matrix_2:List[List[Double]]):Double=
{
  def scalarProd(mat1:List[Double],mat2:List[Double]):Double=
  {
  if(mat1.nonEmpty) mat1.head*mat2.head + scalarProd(mat1.tail,mat2.tail)
  else 0
  }
  def dotProdUtil(matrix_1:List[List[Double]],matrix_2:List[List[Double]],accum:Double):Double=
  {
  if(matrix_1.nonEmpty)
  dotProdUtil(matrix_1.tail,matrix_2.tail,accum+scalarProd(matrix_1.head,matrix_2.head))
  else accum
  }
 dotProdUtil(matrix_1,matrix_2,0.0)
}
// making the Convolute layer
def flatten[A](list: List[List[A]]):List[A] = {
    if (list.length==0) List[A]() 
    else list.head ++ flatten(list.tail)
}
def convolute(Image:List[List[Double]], Kernel:List[List[Double]],ImageSize : List[Int],KernelSize:List[Int]):List[List[Double]] = {
    if(ImageSize.head < KernelSize.head)Nil
    else
    {
    def removeColumn(matrix:List[List[Double]]):List[List[Double]] = {
      if(matrix.nonEmpty)
      {
        matrix.head.tail :: removeColumn(matrix.tail)
      }
      else Nil
    }
    def rowList(Image:List[List[Double]], Kernel:List[List[Double]], colnum:Int):List[Double] = {
      if(colnum+KernelSize.tail.head > ImageSize.tail.head)Nil
      else
      {
        dotProduct(Kernel,Image) :: rowList(removeColumn(Image),Kernel,colnum+1) 
      }
    }
    rowList(Image,Kernel,0)::convolute(Image.tail,Kernel,List(ImageSize.head-1,ImageSize.tail.head),KernelSize)
    } 
  }
def activationLayer(activationFunc:Double => Double,Image:List[List[Double]]):List[List[Double]]=
{
  def adjust(list: List[Double],f:Double => Double,accum:List[Double]):List[Double]={
  if(list.nonEmpty){
  adjust(list.tail,f,accum:::List(f(list.head)))
  }else {
  accum
  }
 }
  def activationLayerUtil(Image:List[List[Double]],activationFunc:Double => Double,accum:List[List[Double]]):List[List[Double]]=
  {
   if(Image.nonEmpty)
   activationLayerUtil(Image.tail,activationFunc,accum:::List(adjust(Image.head,activationFunc,Nil)))
   else accum
  }  
  activationLayerUtil(Image,activationFunc,Nil)
}
//Pooling in All
def split(in:List[List[Double]],size:Int): List[List[Double]] = {
       if(in==Nil)
         Nil
       else
       {
        if(size == 0)
        in
        else
        {
           if(in.tail == Nil)
           {
            split(List(List(in.head.head),in.head.tail),size-1)
           }
           else
            split(List(in.head:::List(in.tail.head.head),flatten(in.tail).tail),size-1)
       }
       }
   }   
def multisplit(in:List[List[Double]],out:List[List[Double]],size:Int): List[List[Double]] = {
    if(in==List(List()))
      out
    else
    {
      val s=split(in,size)
      multisplit(s.tail,out:::List(s.head),size)
    }
  }

  def join(l1:List[List[Double]],l2:List[List[Double]],l3:List[List[Double]]):List[List[Double]]= {
      if(l1==Nil && l2!=Nil)
      l2
      else if(l2==Nil && l1!=Nil)
      l1
      else if(l1==Nil && l2==Nil)
      l3
      else
      join(l1.tail,l2.tail,l3:::(List(l1.head:::l2.head)))
  }
  
  def multijoin(m:List[List[Double]],out:List[List[Double]],size:Int,count:Int):List[List[Double]] ={
      if(count==size)
      out
      else if(m==Nil)
        out
      else
      {
          val ms=multisplit(List(m.head),Nil,size)
          multijoin(m.tail,join(out,ms,Nil),size,count+1)
      }
      
  }
    def activatepool(poolingFunc:List[Double]=>Double,m:List[List[Double]],out:List[Double]):List[Double] ={
        if(m==Nil)
            out
        else
            activatepool(poolingFunc,m.tail,out:::List(poolingFunc(m.head)))

    }

     def singlepooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],out:List[Double], size:Int):List[Double] = {
       if(Image==Nil)
       out
       else if(Image.head.size == size)
        List(poolingFunc(flatten(Image)))
       else
       {
       val mj = multijoin(Image,Nil,size,0)
       activatepool(poolingFunc,mj,Nil)
       }
   }
      def singlePooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]], size:Int):List[Double] = {
        singlepooling(poolingFunc,Image,Nil,size)
      }
def rowSkip(Image:List[List[Double]], K:Int):List[List[Double]] = {
  if (K==0)
    Image
  else
    rowSkip(Image.tail, K-1)
}

def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int ):List[List[Double]]={
  if (Image==Nil)
    List()
  else
    List(singlePooling(poolingFunc, Image, K)) ::: poolingLayer(poolingFunc, rowSkip(Image, K), K)
}
// maximum of two matrix
def maxOfTwo(a:Double,b:Double):Double= if(a>b)a else b
def maxOfList(list1:List[Double],accum:Double):Double={
 if(list1.nonEmpty){
 maxOfList(list1.tail,maxOfTwo(list1.head,accum))
 }
 else
 accum
}
def maxmatrix(matrix:List[List[Double]],accum:Double):Double={
 if(matrix.nonEmpty)
 {
  maxmatrix(matrix.tail,maxOfList(matrix.head,accum))
 }
 else accum
}

//finding minimum
def minOfTwo(a:Double,b:Double):Double= if(a<b)a else b
def minOfList(list1:List[Double],accum:Double):Double={
 if(list1.nonEmpty){
 minOfList(list1.tail,minOfTwo(list1.head,accum))
 }
 else accum
}
def minmatrix(matrix:List[List[Double]],accum:Double):Double={
 if(matrix.nonEmpty)
 {
  minmatrix(matrix.tail,minOfList(matrix.head,accum))
 }
 else accum
}
def normalfunc(x:Double,min:Double,max:Double):Int = {
      	val a = ((255*(x-min))/(max-min)).toFloat
    	scala.math.round(a)
    }
    //function to normalise a matrix
    def normaliseListhelp(list:List[Double],min:Double,max:Double):List[Int] = {
    	def nlisthelp(list:List[Double],min:Double,max:Double,acc:List[Int]):List[Int] =  {
    		if(list.isEmpty) acc
    		else nlisthelp(list.tail,min,max,acc:::List(normalfunc(list.head,min,max))) 
    	}
    	nlisthelp(list,min,max,Nil)
    }
    def nmatrixhelp(Image:List[List[Double]],min:Double,max:Double,acc:List[List[Int]]):List[List[Int]] =  {
    		if(Image.isEmpty) acc
    		else nmatrixhelp(Image.tail,min,max,acc:::List(normaliseListhelp(Image.head,min,max)))
    }
def normalisematrixhelp(Image:List[List[Double]],min:Double,max:Double):List[List[Int]] = {
    	nmatrixhelp(Image,min,max,Nil)
}
    // normalising a layer
def normalise(Image:List[List[Double]]):List[List[Int]]={
    	val max = maxmatrix(Image,Image.head.head)
    	val min = minmatrix(Image,Image.head.head)
    	normalisematrixhelp(Image,min,max)
}
//mixed Layer
def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int], activationFunc:Double => Double, poolingFunc:List[Double]=>Double,  K:Int):List[List[Double]] = {
		poolingLayer( poolingFunc, activationLayer(activationFunc, convolute(Image, Kernel, imageSize, kernelSize)), K)
}
//assembly
def addingList(ls1:List[Double],ls2:List[Double]):List[Double]={
 def addingListUtl(ls1:List[Double],ls2:List[Double],accu:List[Double]):List[Double]={
     if(ls1.nonEmpty)
        addingListUtl(ls1.tail,ls2.tail,accu:::List(ls1.head+ls2.head))
    else
        accu  
 }
 addingListUtl(ls1,ls2,Nil)
}

def addingMatrix(matrix1:List[List[Double]],matrix2:List[List[Double]]):List[List[Double]]={
  def addingMatrixUtl(matrix1:List[List[Double]],matrix2:List[List[Double]],accu:List[List[Double]]):List[List[Double]]={ 
  if(matrix1.nonEmpty)
    addingMatrixUtl(matrix1.tail,matrix2.tail,accu:::List(addingList(matrix1.head,matrix2.head)))  
  else
    accu
  }
  addingMatrixUtl(matrix1,matrix2,Nil)
}
def maxPooling(list1:List[Double]):Double={
 def maxPoolingUtl(list1:List[Double],accu:Double):Double= list1 match{
  case x::xs => maxPoolingUtl(xs,findingMaxOfTwo(x,accu))
  case Nil => accu
 }
 maxPoolingUtl(list1,list1.head)
}
def findingMaxOfTwo(a:Double,b:Double):Double= if(a>b)a else b
def avgPooling(list:List[Double]):Double={
    def meanCalc(list:List[Double],sum:Double=0.0,count:Double=0):Double={
      if(list==Nil)
        return sum/count
      meanCalc(list.tail,sum+list.head,count+1)
    }
    meanCalc(list)
  }
def assembly(Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,Kerne11:List[List[Double]],kernelSize1:List[Int],Kernel2:List[List[Double]],kernelSize2:List[Int],Kernel3:List[List[Double]],kernelSize3:List[Int],Size: Int):List[List[Int]]={
 val t1=mixedLayer(Image,Kerne11,imageSize,kernelSize1,(x:Double)=>if(x>0) x else 0,avgPooling,Size)
 val t2=mixedLayer(Image,Kernel2,imageSize,kernelSize2,(x:Double)=>if(x>0) x else 0,avgPooling,Size)

 val a1=activationLayer((x:Double)=>x*w1,t1)
 val a2=activationLayer((x:Double)=>x*w2,t2)
 val a3=addingMatrix(a1,a2)

 val t3=activationLayer((x:Double)=>x+b,a3)
 
 val r= (imageSize.head - kernelSize1.head + 1)/Size
 val c= (imageSize.tail.head - kernelSize1.tail.head + 1)/Size
 val sa3= List(r,c)

 val t4=mixedLayer(t3,Kernel3,sa3,kernelSize3,(x:Double)=>if(x>0)x else 0.5*x,maxPooling,Size)
 normalise(t4)
}
}