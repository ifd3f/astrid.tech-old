import { format } from "date-fns";
import { useRouter } from "next/router";
import { FC, ReactNode, useState } from "react";
import { FaFlag, FaLink, FaReply } from "react-icons/fa";
import { Button } from "reactstrap";
import { Comment, CommentAuthor } from "../../../lib/astrid-tech-api";
import { useCommentData } from "./CommentDataProvider";
import { CommentingForm } from "./CommentingForm";
import { CommentList } from "./CommentList";
import { ReportingForm } from "./ReportingForm";

type CommentNodeProps = {
  comment: Comment;
  isReply?: boolean;
};

type CommentState = "reply" | "report" | null;

const AuthorDisplay: FC<{ author: CommentAuthor }> = ({ author }) => {
  const user = <strong>{author.name ?? "[anonymous]"}</strong>;
  return (
    <>
      {author.website ? <a href={author.website}>{user}</a> : user}
      {author.email ? ` <${author.email}>` : null}
    </>
  );
};

export const CommentNode: FC<CommentNodeProps> = ({
  comment,
  isReply = false,
}) => {
  const { refreshComments } = useCommentData();

  const date = format(comment.timeAuthored, "d MMM yyyy");
  const router = useRouter();
  const commentId = `comment-${comment.id}`;
  const url = new URL(window.location.href);
  url.hash = commentId;

  const [actionState, setActionState] = useState<CommentState>(null);

  const toggleAction = (newState: CommentState) => () => {
    setActionState(actionState == newState ? null : newState);
  };

  const onSubmitted = async () => {
    setActionState(null);
    await refreshComments();
  };

  let form: ReactNode;
  switch (actionState) {
    case null:
      form = null;
      break;
    case "reply":
      form = <CommentingForm replyTo={comment.id} onSubmitted={onSubmitted} />;
      break;
    case "report":
      form = <ReportingForm comment={comment.id} onSubmitted={onSubmitted} />;
      break;
  }

  return (
    <div>
      <hr />
      <article className="comment" id={commentId}>
        <div className="d-flex">
          <div className="header mr-auto">
            <AuthorDisplay author={comment.author} />{" "}
            <span className="text-muted">
              {isReply ? "replied " : null}
              on {date}
            </span>
          </div>
          <div className="">
            <Button
              outline={actionState != "reply"}
              onClick={toggleAction("reply")}
              color="primary"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaReply title="reply" /> Reply
            </Button>{" "}
            <Button
              outline={actionState != "report"}
              onClick={toggleAction("report")}
              color="danger"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaFlag title="flag" /> Report
            </Button>{" "}
            <Button
              href={url.href}
              outline
              color="info"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaLink title="link" /> Permalink
            </Button>
          </div>
        </div>
        <div
          className="body"
          dangerouslySetInnerHTML={{ __html: comment.htmlContent }}
        />
        <div className="children">
          {form}
          <CommentList comments={comment.children} isReply />
        </div>
      </article>
    </div>
  );
};
