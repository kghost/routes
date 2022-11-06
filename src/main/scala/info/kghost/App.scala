package info.kghost

import java.math.BigInteger
import java.net.InetAddress
import org.apache.commons.cli.Options
import org.apache.commons.cli.DefaultParser
import org.apache.commons.cli.HelpFormatter

object App {
  def main(args: Array[String]) {
    val options = new Options();
    options.addOption("h", "help", false, "display help");
    options.addOption("6", "ipv6", false, "process ipv6 prefixes");
    options.addOption("c", "country", true, "filter which country");
    options.addOption("f", "format", true, "output format, cidr or mask");
    options.addOption("r", "reverse", false, "output reverse (negetive) route");

    val parser = new DefaultParser();
    val cmd = parser.parse(options, args);

    if (cmd.hasOption("h")) {
      val formatter = new HelpFormatter();
      formatter.printHelp("rs", options);
    } else {
      val data = Data.data

      val family: Family = if (cmd.hasOption("6")) new V6 else new V4
      val country = cmd.getOptionValue("c", "CN")
      val format = cmd.getOptionValue("f", "mask") match {
        case "cidr" => family.cidr _
        case "mask" => family.mask _
      }
      val reverse = cmd.hasOption("r")

      val (include, exclude) = data.foldLeft((null: BinaryTree, family.exclude)) {
        case ((include, exclude), line) => family.lineToTree(line) match {
          case Some((loc, tree)) => {
            if ((loc == country) ^ reverse)
              (family.union(include, tree), exclude)
            else
              (include, family.union(exclude, tree))
          }
          case _ => (include, exclude)
        }
      }
      family.print(family.optimize(include, exclude), format)
    }
  }
}
