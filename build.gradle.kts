/*
 * This file was generated by the Gradle 'init' task.
 *
 * This generated file contains a sample Scala library project to get you started.
 * For more details take a look at the Scala plugin chapter in the Gradle
 * user guide available at https://docs.gradle.org/5.1/userguide/scala_plugin.html
 */

plugins {
	idea
}

val scalaVersion: String by extra("2.11.8") //2.12.7

val botProjects: List<String> by extra(listOf("DeepThought", "BreadcrumbBot", "KewlBot", "ReferenceBot", "Overmind", "ScourgeOfScala"))

subprojects {
 
	apply(plugin = "scala")

	repositories {
		mavenCentral()
		jcenter()

		//maven { url = uri("http://repo.typesafe.com/typesafe/releases/") }
	}



	dependencies {

		"implementation"("org.scala-lang:scala-library:$scalaVersion")

		// Use Scalatest for testing our library
		//"testImplementation"("junit:junit:4.12")
		//"testImplementation"("org.scalatest:scalatest_2.12:3.0.5")

		// Need scala-xml at test runtime
		//"testRuntimeOnly"("org.scala-lang.modules:scala-xml_2.12:1.1.1")
	}
}

project(":ScalatronCore") {

	dependencies {
		"implementation"("com.typesafe.akka:akka-actor_2.11:2.3.16")
	}
}

project(":BotWar") {

	dependencies {
		"implementation"("com.typesafe.akka:akka-actor_2.11:2.3.16")
		"implementation"(project(":ScalatronCore"))
	}
}

project(":Scalatron") {

	dependencies {

		"implementation"("com.typesafe.akka:akka-actor_2.11:2.3.16")
		"implementation"("org.eclipse.jetty.aggregate:jetty-webapp:7.6.2.v20120308")
		"implementation"("org.eclipse.jgit:org.eclipse.jgit:1.3.0.201202151440-r")
		"implementation"("org.eclipse.jgit:org.eclipse.jgit.http.server:1.3.0.201202151440-r")
		"implementation"("org.codehaus.jackson:jackson-jaxrs:1.9.2")
		"implementation"("com.sun.jersey:jersey-bundle:1.12")
		"implementation"("javax.servlet:servlet-api:2.5")

		//compiler
		"implementation"("org.scala-lang:scala-compiler:$scalaVersion") //2.9.1

		//test dependencies
		"testImplementation"("org.scalatest:scalatest_2.12:3.0.5")
		"testImplementation"("org.testng:testng:6.5.1")
		"testImplementation"("org.specs2:specs2_2.11:3.7")

		//project dependencies
		"implementation"(project(":ScalatronCore"))
		"implementation"(project(":BotWar"))

		//bots
		"implementation"(project(":ReferenceBot"))
		"implementation"(project(":KewlBot"))
		"implementation"(project(":DeepThought"))
		//"implementation"(project(":Overmind"))
		//"implementation"(project(":ScourgeOfScala"))
		//"implementation"(project(":BreadcrumbBot"))
		"implementation"(project(":aibot"))
	}
}

project(":ScalatronCLI") {
	dependencies {
		"implementation"("org.apache.httpcomponents:httpclient:4.1.3")
		"implementation"("org.scala-lang.modules:scala-parser-combinators_2.11:1.1.0")
	}
}

/*
configure(subprojects.filter { botProjects.contains(it.name) }) {
	dependencies {
		"testImplementation"("org.scalatest:scalatest_2.12:3.0.5")
		"testImplementation"("org.specs2:specs2_2.11:3.7")
	}
}
*/

project(":ReferenceBot") {
	dependencies {
		"implementation"(project(":ScalatronCore"))
	}
}

project(":aibot") {

	val dl4jVersion: String by extra("1.0.0-beta4")

	dependencies{
		"implementation"("org.deeplearning4j:scalnet_2.11:$dl4jVersion")
		//"implementation"("org.deeplearning4j:deeplearning4j-core:$dl4jVersion")
		"implementation"("org.nd4j:nd4j-native-platform:$dl4jVersion")
		//"implementation"("org.nd4j:nd4j-cuda-10.1:$dl4jVersion")

		//"testImplementation"("org.scalatest:scalatest_2.12:3.0.5")
		"testImplementation"("org.specs2:specs2_2.11:3.7")
	}
}
