package pplAssignment

object F2016A7PS0150P{
    //Start Coding from here
    def dotProduct(matrix_1:List[List[Double]], matrix_2:List[List[Double]]):Double = {
	def helper_1(matrix_1:List[List[Double]], matrix_2:List[List[Double]], accumulator:Double):Double = {
		if(matrix_1.isEmpty)
			accumulator;
		else
			helper_1(matrix_1.tail, matrix_2.tail, helper_2(matrix_1.head, matrix_2.head, 0.0)+accumulator);
        }

	def helper_2(l1:List[Double], l2:List[Double], accumulator:Double):Double = {
		if(l1.isEmpty)
			accumulator;
		else
			helper_2(l1.tail, l2.tail, l1.head*l2.head+accumulator);
        }
	helper_1(matrix_1, matrix_2, 0.0);
    }

    def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]):List[List[Double]] = {
	val i_r = imageSize.head;
	val i_c = imageSize.tail.head;
	val k_r = kernelSize.head;
	val k_c = kernelSize.tail.head;
	/*def help_1(Image:List[List[Double]] /*,Kernel:List[List[Double]]*/):List[List[Double]] = {
		if(Image.length == k_r)
			List(help_2(Image, Kernel, 0, List()));
		else
			List(help_2(Image, Kernel, 0, List())):::help_1(Image.tail /*,Kernel*/);
        }*/

	def help_1(Image:List[List[Double]] /*,Kernel:List[List[Double]]*/, l:List[List[Double]]):List[List[Double]] = {
		if(Image.length == k_r)
			l:::List(help_2(Image /*,Kernel*/, 0, List()));
		else
			help_1(Image.tail, l:::List(help_2(Image /*,Kernel*/, 0, List())));
        }

	/*def help_2(Image:List[List[Double]], Kernel:List[List[Double]], cnt:Int):List[Double] = {
		val l:List[List[Double]] = help_3(Image, cnt);
		if(cnt+k_c == i_c)
			List(dotProduct(l, Kernel));
		else
			List(dotProduct(l, Kernel)):::help_2(Image, Kernel, cnt+1);
        }*/

	def help_2(Image:List[List[Double]], /*Kernel:List[List[Double]],*/ cnt:Int, acc:List[Double]):List[Double] = {
		val l:List[List[Double]] = help_3(Image, cnt);
		if(cnt+k_c == i_c)
			acc:::List(dotProduct(l, Kernel));
		else
			help_2(Image /*,Kernel*/, cnt+1, acc:::List(dotProduct(l, Kernel)));
        }


	def help_3(Image:List[List[Double]], cnt:Int):List[List[Double]] = {
		def help_3_1(Image:List[List[Double]], rec:Int, l:List[List[Double]]):List[List[Double]] = {
			if(rec == k_r)
				l;
			else
				help_3_1(Image.tail, rec+1, l:::List(help_4(Image.head, cnt, 0, List())));
		}
		help_3_1(Image, 0, List());
	}

	def help_4(l:List[Double], cnt:Int, acc:Int, r:List[Double]):List[Double] = {
		if(acc<cnt)
			help_4(l.tail, cnt, acc+1,r);
		else if(acc<cnt+k_c-1)
			help_4(l.tail, cnt, acc+1,r:::List(l.head));
		else
			r:::List(l.head);
	}

	val l:List[List[Double]] = List();
	help_1(Image /*,Kernel*/,l);
    }

    def activationLayer(activationFunc:Double => Double, Image:List[List[Double]]):List[List[Double]] = {
	def helper_1(Image:List[List[Double]], acc:List[List[Double]]):List[List[Double]] = {
		if(Image.isEmpty)
			acc;
		else
			helper_1(Image.tail, acc:::List(helper_2(Image.head, List())));
        }

	def helper_2(l:List[Double], acc:List[Double]):List[Double] = {
		if(l.isEmpty)
			acc;
		else
			helper_2(l.tail, acc:::List(activationFunc(l.head)));
        }
	helper_1(Image, List());
    }

    def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int):List[Double] = {
	val M = Image.head.length;
	def helper_1(cnt:Int, l:List[Double]):List[Double] = {
		if(cnt+K == M)
			l:::List(poolingFunc(helper_2(Image, cnt)));
		else
			helper_1(cnt+K, l:::List(poolingFunc(helper_2(Image, cnt))));
	}

	def helper_2(Image:List[List[Double]], cnt:Int):List[Double] = {
		def helper_2_1(Image:List[List[Double]], l:List[Double]):List[Double] = {
			if(Image.isEmpty)
				l;
			else
				helper_2_1(Image.tail, l:::helper_3(Image.head, cnt, 0, List()));

		}
		helper_2_1(Image, List());
	}


	def helper_3(l:List[Double], cnt:Int, acc:Int, r:List[Double]):List[Double] = {
		if(acc<cnt)
			helper_3(l.tail, cnt, acc+1,r);
		else if(acc<cnt+K-1)
			helper_3(l.tail, cnt, acc+1,r:::List(l.head));
		else
			r:::List(l.head);
	}
	helper_1(0, List());
    }

    /*def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int):List[List[Double]] = {
	def help_1(Image:List[List[Double]], l:List[List[Double]]):List[List[Double]] = {
		val It, Ih = help_2(0, Image, List());
		if(It.isEmpty)
			l:::List(singlePooling(poolingFunc, Ih, K));
		else
			help_1(It, l:::List(singlePooling(poolingFunc, Ih, K)));
	}

	def help_2(cnt:Int, It:List[List[Double]], Ih:List[List[Double]]):(List[List[Double]], List[List[Double]]) = {
		if(cnt == K)
			(It, Ih);
		else
			help_2(cnt+1, It.tail, Ih:::List(It.head));
	}
	help_1(Image, List());
    }*/

    def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int):List[List[Double]] = {
	def help_1(Image:List[List[Double]], l:List[List[Double]]):List[List[Double]] = {
	    val I = help_2(0, Image, List());
	    val Ih = I.head;
	    val It = I.tail.head;
	    if(It.isEmpty)
			l:::List(singlePooling(poolingFunc, Ih, K));
		else
			help_1(It, l:::List(singlePooling(poolingFunc, Ih, K)));
	}

	def help_2(cnt:Int, It:List[List[Double]], Ih:List[List[Double]]):List[List[List[Double]]] = {
		if(cnt == K)
			List(Ih, It);
		else
			help_2(cnt+1, It.tail, Ih:::List(It.head));
	}
	help_1(Image, List());
    }

    def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int], activationFunc:Double => Double,
		   poolingFunc:List[Double]=>Double, K:Int):List[List[Double]] ={
		val I_1 = convolute(Image, Kernel, imageSize, kernelSize);
		val I_2 = activationLayer(activationFunc, I_1);
		val I_3 = poolingLayer(poolingFunc, I_2, K);
		I_3;
    }

    //val d:Double = 12.5;
    //val i:Int = Math.round(d).toInt;

    /*def normalise(Image:List[List[Double]]):List[List[Int]] = {
	val max:Double = help_2((a:Double, b:Double)=> a>b, Image.tail, help_3((a:Double, b:Double)=> a>b, Image.head.tail, Image.head.head));
	val min:Double = help_2((a:Double, b:Double)=> a<b, Image.tail, help_3((a:Double, b:Double)=> a<b, Image.head.tail, Image.head.head));
	val max_min = max-min;

	def help_1_1(Image:List[List[Double]], r:List[List[Int]]):List[List[Int]] = {
		if(Image.isEmpty)
			r;
		else
			help_1_1(Image.tail, r:::List(help_1_2(Image.head, List())));
	}

	def help_1_2(l:List[Double], r:List[Int]):List[Int] = {
		if(l.isEmpty)
			r;
		else
			help_1_2(l.tail, r::Math.round((((l.head-min)/max_min)*255)).toInt;
	}


	def help_2(f:(Double, Double)=>Boolean, l:List[List[Double]], acc:Double):Double = {
		if(l.isEmpty)
			acc;
		else{
			val temp:Double = help_3(f, l.head.tail, l.head.head);
			if(f(acc,temp)
				help_2(f, l.tail, acc);
			else
				help_2(f, l.tail, temp);
		}
	}

	def help_3(f:(Double, Double)=>Boolean, l:List[Double], acc:Double):Double = {
		if(l.isEmpty)
			acc;
		else{
			if(f(acc, l.head))
				help_3(f, l.tail, acc);
			else
				help_3(f, l.tail, l.head);
		}
	}

	help_1_1(Image, List());
    }*/

    def normalise(Image:List[List[Double]]):List[List[Int]] = {

	def help_2(f:(Double, Double)=>Boolean, l:List[List[Double]], acc:Double):Double = {
		if(l.isEmpty)
			acc;
		else{
			val temp:Double = help_3(f, l.head.tail, l.head.head);
			if(f(acc,temp))
				help_2(f, l.tail, acc);
			else
				help_2(f, l.tail, temp);
		}
	}

	def help_3(f:(Double, Double)=>Boolean, l:List[Double], acc:Double):Double = {
		if(l.isEmpty)
			acc;
		else{
			if(f(acc, l.head))
				help_3(f, l.tail, acc);
			else
				help_3(f, l.tail, l.head);
		}
	}

	val max:Double = help_2(((a:Double, b:Double)=> a>b), Image.tail, help_3((a:Double, b:Double)=> a>b, Image.head.tail, Image.head.head));
	val min:Double = help_2((a:Double, b:Double)=> a<b, Image.tail, help_3((a:Double, b:Double)=> a<b, Image.head.tail, Image.head.head));
	val max_min:Double = max-min;

	def help_1_1(Image:List[List[Double]], r:List[List[Int]]):List[List[Int]] = {
		if(Image.isEmpty)
			r;
		else
			help_1_1(Image.tail, r:::List(help_1_2(Image.head, List())));
	}

	def help_1_2(l:List[Double], r:List[Int]):List[Int] = {
		if(l.isEmpty)
			r;
		else
			help_1_2(l.tail, r:::List(Math.round((((l.head-min)/max_min)*255)).toInt));
	}

	help_1_1(Image, List());
    }

    def assembly(Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]], kernelSize1:List[Int], 			 Kernel2:List[List[Double]],kernelSize2:List[Int],Kernel3:List[List[Double]],kernelSize3:List[Int],Size:Int):List[List[Int]] ={
	val t1:List[List[Double]] = mixedLayer(Image, Kernel1, imageSize, kernelSize1, (d:Double)=> if(d>0) d else 0, avg_pooling, Size);
	val t2:List[List[Double]] = mixedLayer(Image, Kernel2, imageSize, kernelSize2, (d:Double)=> if(d>0) d else 0, avg_pooling, Size);
	val temp1:List[List[Double]] = scalar_mat_op(t1, w1, (a:Double, b:Double)=>a*b);
	val temp2:List[List[Double]] = scalar_mat_op(t2, w2, (a:Double, b:Double)=>a*b);
	val temp3:List[List[Double]] = mat_add(temp1, temp2);
	val t3:List[List[Double]] = scalar_mat_op(temp3, b, (a:Double, c:Double)=>a+c);
	val t4:List[List[Double]] = mixedLayer(t3, Kernel3, List(t3.length, t3.head.length), kernelSize3, (d:Double)=> if(d>0) d else 0.5*d, max_pooling, Size);
	val out:List[List[Int]] = normalise(t4);
	out;
    }

    def scalar_mat_op(l:List[List[Double]], d:Double, f:(Double, Double)=>Double):List[List[Double]] = {
	def help_1(l:List[List[Double]], r:List[List[Double]]):List[List[Double]] = {
		if(l.isEmpty)
			r;
		else
			help_1(l.tail, r:::List(help_2(l.head, List())));
	}
	def help_2(l:List[Double], r:List[Double]):List[Double] = {
		if(l.isEmpty)
			r;
		else
			help_2(l.tail, r:::List(f(l.head,d)));
	}
	help_1(l, List());
    }

    def mat_add(m1:List[List[Double]], m2:List[List[Double]]):List[List[Double]] = {
	def help_1(m1:List[List[Double]], m2:List[List[Double]], r:List[List[Double]]):List[List[Double]] = {
		if(m1.isEmpty)
			r;
		else
			help_1(m1.tail, m2.tail, r:::List(help_2(m1.head, m2.head, List())));
	}

	def help_2(l1:List[Double], l2:List[Double], r:List[Double]):List[Double] = {
		if(l1.isEmpty)
			r;
		else
			help_2(l1.tail, l2.tail, r:::List((l1.head+l2.head)));
	}
	help_1(m1, m2, List());
    }

    def ReLU(d:Double):Double = {
	if(d>0)
		d;
	else
		0;
    }


    def avg_pooling(l:List[Double]):Double = {
	val len = l.length;
	def help(l:List[Double], acc:Double):Double = {
		if(l.isEmpty)
			acc/len;
		else
			help(l.tail, acc+l.head);
	}
	help(l, 0.0);
    }

    def max_pooling(l:List[Double]):Double ={
	def help(l:List[Double], acc:Double):Double = {
		if(l.isEmpty)
			acc;
		else{
			if(acc>l.head)
				help(l.tail, acc);
			else
				help(l.tail, l.head);
		}
	}
	help(l.tail, l.head);
    }

}
