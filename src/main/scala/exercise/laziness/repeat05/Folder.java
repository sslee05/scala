package exercise.laziness.repeat05;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Folder {

	public static  <A,B> List<B> fold(List<A> xs,B z, BiFunction<A,B,B> fn)  {
		List<B> rx = new ArrayList<>();
		B init = z;

		for(A a : xs) {
			init = fn.apply(a,init);
			rx.add(init);
		}
		return rx;
	}

	public static void main(String[] args) {
		List<Integer> xs = Arrays.asList(1,2,3,4,5);
		List<String> rx = fold(xs,"",(b,a) -> a + b);
		rx.forEach(System.out::println);

		List<Integer> sx = Arrays.asList(1,2,3,4,5,6,7);


		User user = sx.stream().collect(
			Collectors.<Integer,User>reducing(new User(""),a -> new User(String.valueOf(a)), (a,b) -> new User(a.userName+b.userName)));
		System.out.println(user.userName);

		List<User> users = sx.stream().map(i -> new User(String.valueOf(i))).collect(Collectors.toList());
		users.stream().forEach(u -> System.out.println(u.userName));

		User user02 = sx.stream().map(i -> new User(String.valueOf(i))).reduce(new User(""),(a,b) -> new User(a.userName+b.userName));
		System.out.println(user02.userName);

	}

	public static class User {
		private String userName;

		public User(String userName) {
			this.userName = userName;
		}

		public String getUserName() {
			return this.userName;
		}
	}


}
