object TypeClasses extends App:
	trait Mask[A]:
		def disclose(a: A): String

	object Mask:
		def maskNothing[A]: Mask[A] =
			(a: A) => a.toString

		given defaultMaskEvertything[A]: Mask[A] with
			def disclose(a: A) = "<secret>"

	case class BankNumber(bankNumber: String)
	case class Customer(name: String, bankNumber: BankNumber)

	object BankNumber:
		given maskBankNumber: Mask[BankNumber] with
			def disclose(bankNumber: BankNumber): String = "BankNumber <secret>"

	object Customer:
		given maskCustomer(using bnMask: Mask[BankNumber]): Mask[Customer] with
			def disclose(c: Customer): String = s"Customer: Name ${c.name}, ${bnMask.disclose(c.bankNumber)}"

	def genericLogService[A](a: A)(using mask: Mask[A]): Unit =
		println(mask.disclose(a))

	val customer: Customer = Customer("Thijs", BankNumber("IBAN123456"))

	genericLogService(customer)(using Mask.maskNothing) //  Customer(Thijs,BankNumber(IBAN123456))
	genericLogService(customer) // Customer: Name Thijs, BankNumber <secret>

	case class Config(id: Int)

	def renderWebsite(path: String)(using Config): String =
		renderElement("header", path) + renderElement("footer", path)

	def renderElement(name: String, path: String)(using c: Config): String =
		s"<${name}>${path}/${c.id}</${name}>"

	val config: Config = Config(123)

	trait Adder[A]:
		def add(x: A, y: A): A

	object Adder:
		given intAdder: Adder[Int] with
			def add(x: Int, y: Int): Int = x + y

		given stringAdder: Adder[String] with
			def add(x: String, y: String): String = (x.toInt + y.toInt).toString

		def genericAdder[A](x: A, y: A)(using adder: Adder[A]): A =
			adder.add(x, y)
